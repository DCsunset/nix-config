(use-package ellama
  :commands (make-llm-ollama)
  :init
  (require 'llm-ollama)
  (setopt ellama-provider
		      (make-llm-ollama
           :host "ollama.local"
           :port 80
		       :chat-model "llama3:8b-instruct-q5_K_M"
		       :embedding-model "llama3:8b-text-q5_K_M")))

