decompiling
instructionsForDecompiling
	"return all instructions, but skip the block bodies, as the decompiler
	recurses over blocks"
	
	^startSequence instructionsForDecompiling allButFirst.