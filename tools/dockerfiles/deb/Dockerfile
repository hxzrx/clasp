FROM image():tag()

SHELL ["/bin/bash", "-c"]

ARG D_USER=app
ARG D_UID=1000

ENV DEBIAN_FRONTEND=noninteractive
ENV USER ${D_USER}
ENV HOME /home/${D_USER}
ENV JUPYTER_PATH=${HOME}/.local/share/jupyter/
ENV JUPYTERLAB_DIR=${HOME}/.local/share/jupyter/lab/
ENV PATH "${HOME}/.local/bin:${PATH}"

RUN apt-get update
RUN apt-get dist-upgrade -y
RUN apt-get install -o Dpkg::Options::="--force-overwrite" -y \
	build-essential clang-13 curl gettext git gpg jq libboost-graph-dev \
	libbsd0 libbsd-dev libclang-13-dev libelf1 libelf-dev libffi-dev \
	libgc-dev libgmp10 libgmp-dev libgmpxx4ldbl libncurses6 libncurses-dev \
	llvm-13 llvm-13-dev locales lsb-release nano python3 python-is-python3 \
	sbcl ssh sudo wget zlib1g zlib1g-dev dh-make equivs devscripts

RUN echo 'en_US.UTF-8 UTF-8' >/etc/locale.gen
RUN sudo locale-gen

RUN useradd --create-home --shell=/bin/false --uid=${D_UID} ${D_USER} && \
    usermod -aG sudo $D_USER && \
    passwd -d $D_USER

WORKDIR ${HOME}
USER ${D_USER}

RUN wget https://beta.quicklisp.org/quicklisp.lisp && \
    sbcl --non-interactive --load quicklisp.lisp \
         --eval '(quicklisp-quickstart:install)' \
         --eval '(ql-util:without-prompting (ql:add-to-init-file))' && \
    rm quicklisp.lisp

ADD --chown=${D_USER}:${D_USER} . clasp
