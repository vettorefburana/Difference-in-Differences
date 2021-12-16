FROM rocker/verse:4.1.0

RUN apt-get clean all && \
  apt-get -o Acquire::Max-FutureTime=86400 update && \
  apt-get upgrade -y && \
  apt-get install -y \
    libhdf5-dev \
    libcurl4-gnutls-dev \
    libssl-dev \
    libxml2-dev \
    libpng-dev \
    libxt-dev \
    zlib1g-dev \
    libbz2-dev \
    liblzma-dev \
    libglpk40 \
    libgit2-28 \
  && apt-get clean all && \
  apt-get purge && \
  rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

WORKDIR /app

ADD requirements.r .
 
RUN Rscript requirements.r

RUN tlmgr update --self --all && \
  tlmgr install fancyhdr multirow listings fancyvrb  \
  xcolor amsmath hyperref infwarerr pdftexcmds hycolor gettitlestring \
  pdftexcmds uniquecounter fancyvrb stringenc rerunfilecheck \
  zapfding refcount pdfescape letltxmacro \
  geometry etexcmds bitset bigintcalc auxhook xcolor framed \
  etoolbox kvsetkeys ltxcmds ltxcmds iftex latex-amsmath-dev epstopdf-pkg \
  eurosym float booktabs multirow tabu enumitem bbm \
  threeparttable threeparttablex ulem makecell caption \
  fontspec tipa unicode-math xunicode bbm-macros wrapfig \ 
  kvoptions colortbl environ trimspaces mdwtools koma-script newfloat pdflscape 

COPY .Rprofile /app

