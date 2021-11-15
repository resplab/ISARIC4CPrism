FROM opencpu/base
RUN R -e 'install.packages("remotes")'
RUN R -e 'remotes::install_github("resplab/isaric4c")'
RUN R -e 'remotes::install_github("resplab/isaric4cPrism")'
RUN echo "opencpu:opencpu" | chpasswd
