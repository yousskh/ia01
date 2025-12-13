FROM clfoundation/sbcl:latest

RUN apt-get update \
 && apt-get install -y curl ca-certificates sqlite3 libsqlite3-dev \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app

COPY app.lisp /app/app.lisp
COPY static /app/static

# Dossier de DB (persistant si tu montes un volume; sur Render ce sera éphémère en free, OK pour démo)
RUN mkdir -p /app/data

# Quicklisp + libs Lisp
RUN curl -L -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive \
    --load /tmp/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install)' \
    --eval '(ql:add-to-init-file)' \
    --eval '(ql:quickload :hunchentoot)' \
    --eval '(ql:quickload :cl-json)' \
    --eval '(ql:quickload :dbi)' \
    --eval '(ql:quickload :dbd-sqlite3)' \
    --eval '(ql:quickload :uiop)' \
    --eval '(quit)'

CMD ["sbcl", "--script", "/app/app.lisp"]
