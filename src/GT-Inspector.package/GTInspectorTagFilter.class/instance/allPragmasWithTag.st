accessing
allPragmasWithTag

	^ PragmaCollector allSystemPragmas select: [ :pragma | pragma key == self tagPragmaKey ]