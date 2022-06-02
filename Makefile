OUT         = interpreter
BUILD_DIR   = build
BNFC        = bnfc
BNFC_OPTS   = -o ${BUILD_DIR} --haskell --functor -m
GHC         = ghc
SRC_DIR     = src
GHC_OPTS    = --make -O2 -outputdir ${BUILD_DIR} -i${SRC_DIR} -i${BUILD_DIR} -Wall -o ${OUT}

.PHONY: all interpreter grammar clean

all: interpreter

interpreter: src/Main.hs
	${GHC} ${GHC_OPTS} $<

grammar: barraquito.cf
	${BNFC} ${BNFC_OPTS} $<
	make -C ${BUILD_DIR}

clean:
	rm -r ${BUILD_DIR}
