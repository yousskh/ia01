FROM clfoundation/sbcl:latest

RUN apt-get update \
 && apt-get install -y curl ca-certificates sqlite3 \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /app
COPY app.lisp /app/app.lisp
COPY db-init.lisp /app/db-init.lisp
COPY static /app/static
RUN mkdir -p /app/data

RUN curl -L -o /tmp/quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp \
 && sbcl --non-interactive \
    --load /tmp/quicklisp.lisp \
    --eval '(quicklisp-quickstart:install :path "/root/quicklisp")' \
    --eval '(quit)'

RUN sbcl --non-interactive \
    --load /root/quicklisp/setup.lisp \
    --eval '(ql:quickload :hunchentoot)' \
    --eval '(ql:quickload :cl-json)' \
    --eval '(ql:quickload :uiop)' \
    --eval '(quit)'

CMD ["sh", "-c", "rm -rf /app/data && mkdir -p /app/data && sbcl --script /app/app.lisp"]
