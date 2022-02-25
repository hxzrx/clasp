(c:recurse #P"extensions/*/"
           #P"include/"
           #P"src/")

(c:includes #~"")

(c:library "gmpxx" :required t :min-version "6.0.0")

(c:library "libffi" :required t :min-version #+bsd "3.3-rc1" #-bsd "3.0.0")

(c:library "bdw-gc" :required t :min-version "7.0.0" :gc :boehm)

(c:library "zlib" :required t :min-version "1.0.0")

#-darwin (c:library "libelf" :required t :min-version #+bsd "0.8.13" #-bsd ".183")

(c:library "ncurses" :required t :min-version "5.7.0")

#-bsd (c:library "libbsd" :required t :min-version "0.10.0")

