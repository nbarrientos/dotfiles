;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=81379
(defun my/monkeys--emacs-sql-bug-81379 nil
  (defun sql-auth-source-search-wallet (wallet product user server database port)
    "Read auth source WALLET to locate the USER secret.
Sets `auth-sources' to WALLET and uses `auth-source-search' to locate the entry.
The DATABASE and SERVER are concatenated with a slash between them as the
host key."
    (let* ((auth-sources wallet)
           host
           secret h-secret sd-secret)

      ;; product
      (setq product (symbol-name product))

      ;; user
      (setq user (unless (string-empty-p user) user))

      ;; port
      (setq port
            (when (and port (numberp port) (not (zerop port)))
              (number-to-string port)))

      ;; server
      (setq server (unless (string-empty-p server) server))

      ;; database
      (setq database (unless (string-empty-p database) database))

      ;; host
      (setq host (if server
                     (if database
                         (concat server "/" database)
                       server)
                   database))

      ;; Perform search
      (dolist (s (auth-source-search :max 1000 :host host))
        (when (and
               ;; Is PRODUCT specified, in the entry, and they are equal
               (if product
                   (if (plist-member s :product)
                       (equal (plist-get s :product) product)
                     t)
                 t)
               ;; Is USER specified, in the entry, and they are equal
               (if user
                   (if (plist-member s :user)
                       (equal (plist-get s :user) user)
                     t)
                 t)
               ;; Is PORT specified, in the entry, and they are equal
               (if port
                   (if (plist-member s :port)
                       (equal (plist-get s :port) port)
                     t)
                 t))
          ;; Is HOST specified, in the entry, and they are equal
          ;; then the H-SECRET list
          (if (and host
                   (plist-member s :host)
                   (equal (plist-get s :host) host))
              (push s h-secret)
            ;; Are SERVER and DATABASE specified, present, and equal
            ;; then the SD-SECRET list
            (if (and server
                     (plist-member s :server)
                     database
                     (plist-member s :database)
                     (equal (plist-get s :server) server)
                     (equal (plist-get s :database) database))
                (push s sd-secret)
              ;; Is SERVER specified, in the entry, and they are equal
              ;; then the base SECRET list
              (if (and server
                       (plist-member s :server)
                       (equal (plist-get s :server) server))
                  (push s secret)
                ;; Is DATABASE specified, in the entry, and they are equal
                ;; then the base SECRET list
                (if (and database
                         (plist-member s :database)
                         (equal (plist-get s :database) database))
                    (push s secret)))))))
      (setq secret (or h-secret sd-secret secret))

      ;; If we found a single secret, return the password
      (when (= 1 (length secret))
        (setq secret (car secret))
        (if (plist-member secret :secret)
            (plist-get secret :secret)
          nil)))))
