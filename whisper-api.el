;;; whisper-api --- Asynchronous Speech-to-Text via Whisper API -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Youseok Yang
;;
;; Author: Youseok Yang <ileixe@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1"))
;; Keywords: multimedia, convenience
;; URL: https://github.com/ileixe/whisper-api.el
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; This package implements an asynchronous speech-to-text interface using
;; OpenAI's Whisper API. Audio is captured by ffmpeg from the default PulseAudio
;; input (on Linux) and stored in a temporary .wav file (with 16000 Hz sample rate).
;;
;; Usage:
;;
;; 1. Set your API key:
;;
;;      (setq whisper-api-openai-token "sk-...")
;;
;; 2. To start recording, run:
;;
;;      M-x whisper-api-record-dwim
;;
;;    The temporary file path is displayed in the minibuffer.
;;
;; 3. To stop recording (and have the file sent to Whisper), run
;;
;;      M-x whisper-api-record-dwim        (again).
;;
;;    The process is stopped via an interrupt so that ffmpeg flushes its buffers
;;    gracefully. The asynchronous call (via curl) is then launched.
;;
;; 4. The API response (the transcription) is then inserted in the target buffer.
;;
;; 5. To cancel an operation (i.e. cancel an active recording or pending curl process),
;;    use:
;;
;;      M-x whisper-api-cancel
;;
;; Debugging messages (using message) are included to help diagnose issues.
;;
;;; Code:

(eval-when-compile (require 'cl-lib))
(require 'json)

(defgroup whisper-api nil
  "Asynchronous speech-to-text using OpenAI Whisper API with cancellation support."
  :group 'multimedia)

;; === User Options ===

(defcustom whisper-api-openai-token nil
  "Your OpenAI API token for Whisper transcription.
Obtain one at https://platform.openai.com/account/api-keys."
  :group 'whisper-api
  :type 'string)

(defcustom whisper-api-max-recording-seconds 180
  "Maximum duration (in seconds) for recording audio."
  :group 'whisper-api
  :type 'integer)

;; Note: Use -ar 16000 to force a 16 kHz sample rate.
(defcustom whisper-api-ffmpeg-command
  "ffmpeg -y -t %d -f pulse -i default -ar 16000 %s"
  "Command template for ffmpeg to record audio.
%d is replaced with the max duration (in seconds) and %s with the temporary file path.
Adjust if you need a different input device (for example, on macOS use avfoundation)."
  :group 'whisper-api
  :type 'string)

;; === Internal Variables ===

(defvar whisper-api--ffmpeg-process nil
  "Active ffmpeg recording process.")

(defvar whisper-api--temp-file nil
  "Temporary file where audio is recorded.")

(defvar whisper-api--stop-requested nil
  "Flag set when the user stops recording (intending to send the file).")

(defvar whisper-api--cancelled nil
  "Flag set when the user cancels the operation.
If non-nil, the ffmpeg sentinel will not send the file to the API.")

;; === Recording Functions ===

(defun whisper-api--check-ffmpeg ()
  "Ensure ffmpeg is installed.
If not, signal an error with installation instructions."
  (unless (executable-find "ffmpeg")
    (error "ffmpeg not found. Please install it (e.g. sudo apt update && sudo apt install ffmpeg)")))

;;;###autoload
(defun whisper-api-record-dwim ()
  "Toggle recording for speech-to-text.
Starts audio capture using ffmpeg if no recording is active.
When called while recording, stops capture and sends the file asynchronously
to OpenAI's Whisper API (unless cancelled)."
  (interactive)
  (if (and whisper-api--ffmpeg-process (process-live-p whisper-api--ffmpeg-process))
      (whisper-api--stop-recording)
    (whisper-api--start-recording)))

(defun whisper-api--start-recording ()
  "Start recording audio to a temporary .wav file.
Resets cancellation flags and launches ffmpeg using `whisper-api-ffmpeg-command'."
  (whisper-api--check-ffmpeg)
  (setq whisper-api--cancelled nil)
  (setq whisper-api--temp-file (make-temp-file "whisper-api-" nil ".wav"))
  (message "Recording audio; temporary file: %s" whisper-api--temp-file)
  (let* ((command (format whisper-api-ffmpeg-command
                          whisper-api-max-recording-seconds
                          (shell-quote-argument whisper-api--temp-file)))
         (proc (start-process-shell-command "whisper-api-ffmpeg"
                                            "*whisper-api-ffmpeg*"
                                            command)))
    (setq whisper-api--ffmpeg-process proc)
    (message "Recording...")
    (set-process-sentinel proc 'whisper-api--ffmpeg-sentinel)))

(defun whisper-api--stop-recording ()
  "Stop the ffmpeg recording process gracefully.
Uses `interrupt-process' to send SIGINT so that ffmpeg flushes its buffers."
  (when (and whisper-api--ffmpeg-process (process-live-p whisper-api--ffmpeg-process))
    (setq whisper-api--stop-requested t)
    (message "Stopping recording gracefully via interrupt...")
    (interrupt-process whisper-api--ffmpeg-process)))

;;;###autoload
(defun whisper-api-cancel ()
  "Cancel any active recording or pending transcription operation.
Kills the recording and any pending curl process, and sets the
cancellation flag so that no API call is made."
  (interactive)
  (setq whisper-api--cancelled t)
  (when (and whisper-api--ffmpeg-process (process-live-p whisper-api--ffmpeg-process))
    (message "Cancelling recording; killing ffmpeg process.")
    (kill-process whisper-api--ffmpeg-process 'SIGKILL)
    (setq whisper-api--ffmpeg-process nil))
  (dolist (proc (process-list))
    (when (and (string= (process-name proc) "whisper-api-curl")
               (process-live-p proc))
      (message "Cancelling pending curl process.")
      (delete-process proc)
      (when (get-buffer (process-buffer proc))
        (kill-buffer (process-buffer proc)))))
  (setq whisper-api--stop-requested nil)
  (message "Whisper API operation cancelled."))

;; === ffmpeg Process Sentinel ===

(defun whisper-api--ffmpeg-sentinel (proc event)
  "Sentinel for the ffmpeg recording process.
PROC is the ffmpeg process and EVENT is the event string.
If the process exits gracefully (exit code 0 or 255) and the operation
has not been cancelled, the audio file is sent asynchronously to the API."
  (when (memq (process-status proc) '(exit signal))
    (setq whisper-api--ffmpeg-process nil)
    (let ((exitcode (process-exit-status proc)))
      (cond
       ((or (= exitcode 0) (= exitcode 255))
        (if whisper-api--cancelled
            (message "Recording cancelled; not sending file to API.")
          (message "Recording finished gracefully (exit code: %d); sending for transcription." exitcode)
          (whisper-api--call-openai-whisper-async whisper-api--temp-file #'whisper-api--default-callback)))
       (t
        (message "Recording process exited abnormally (exit code: %d)." exitcode)
        (when (file-exists-p whisper-api--temp-file)
          (delete-file whisper-api--temp-file)))))))

;; === Asynchronous API Request via curl ===

(defun whisper-api--call-openai-whisper-async (audio-path callback)
  "Send AUDIO-PATH to OpenAI Whisper API asynchronously.
CALLBACK is a function taking two arguments: the transcription text
and the target buffer. The current (target) buffer is captured for insertion."
  (unless whisper-api-openai-token
    (error "Please set whisper-api-openai-token to your API key."))
  (let ((target-buffer (current-buffer)))
    (let* ((curl-cmd (list "curl" "-s"
                           "https://api.openai.com/v1/audio/transcriptions"
                           "-H" (format "Authorization: Bearer %s" whisper-api-openai-token)
                           "-H" "Content-Type: multipart/form-data"
                           "-F" (format "file=@%s" audio-path)
                           "-F" "model=whisper-1"))
           (buffer-name "*whisper-api-curl*"))
      (when (get-buffer buffer-name)
        (kill-buffer buffer-name))
      (message "Sending audio to Whisper API asynchronously...")
      (let ((proc (apply #'start-process "whisper-api-curl" buffer-name curl-cmd)))
        (set-process-sentinel proc
          (lambda (process event)
            (when (string= event "finished\n")
              (with-current-buffer (process-buffer process)
                (let* ((response (buffer-string))
                       (json-object-type 'alist)
                       (result (condition-case err
                                   (json-read-from-string response)
                                 (error (progn
                                          (message "Error parsing JSON: %S" err)
                                          nil))))
                       (transcription (and result (cdr (assoc 'text result)))))
                  (if transcription
                      (funcall callback transcription target-buffer)
                    (message "No transcription received. Full response: %s" response))))
              (delete-process process)
              (when (get-buffer (process-buffer process))
                (kill-buffer (process-buffer process))))))))))

(defun whisper-api--default-callback (transcription target-buffer)
  "Default callback for handling TRANSCRIPTION.
Inserts TRANSCRIPTION into TARGET-BUFFER and deletes the temporary file."
  (with-current-buffer target-buffer
    (insert "\n" transcription "\n"))
  (when (file-exists-p whisper-api--temp-file)
    (delete-file whisper-api--temp-file)))

(provide 'whisper-api)
