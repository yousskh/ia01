FROM clfoundation/sbcl:latest

RUN apt-get update \
 && apt-get install -y curl ca-certificates sqlite3 libsqlite3-dev \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY app.lisp /app/app.lisp
COPY static /app/static
RUN mkdir -p /app/data

# Installer Quicklisp (sans toucher aux init files)
RUN curl -L -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive \
    --load /tmp/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install :path "/root/quicklisp")' \
    --eval '(quit)'

# Précharger les dépendances (IMPORTANT: charger setup.lisp avant ql:quickload)
RUN sbcl --non-interactive \
    --load /root/quicklisp/setup.lisp \
    --eval '(ql:quickload :hunchentoot)' \
    --eval '(ql:quickload :cl-json)' \
    --eval '(ql:quickload :dbi)' \
    --eval '(ql:quickload :dbd-sqlite3)' \
    --eval '(quit)'

CMD ["sbcl", "--script", "/app/app.lisp"]
