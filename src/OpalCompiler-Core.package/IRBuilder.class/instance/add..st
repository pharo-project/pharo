private
add: instr
	
	"Associate instr with current parse node or byte range"
	instr sourceNode: self sourceNode.
	instr bytecodeIndex: self sourceByteIndex.
	^ currentSequence add: instr