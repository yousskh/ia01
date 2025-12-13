# Image de base : Linux + SBCL déjà installé
FROM clfoundation/sbcl:latest

# On installe curl (pour télécharger Quicklisp)
RUN apt-get update \
 && apt-get install -y curl ca-certificates \
 && rm -rf /var/lib/apt/lists/*

# Dossier de travail à l'intérieur du conteneur
WORKDIR /app

# On copie ton fichier Lisp dans le conteneur
COPY app.lisp /app/app.lisp

# On installe Quicklisp + les bibliothèques nécessaires
RUN curl -L -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive \
    --load /tmp/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:add-to-init-file)' \
    --eval '(ql:quickload :hunchentoot)' \
    --eval '(ql:quickload :cl-json)' \
    --eval '(quit)'

# Commande lancée quand le conteneur démarre
CMD ["sbcl", "--script", "/app/app.lisp"]
