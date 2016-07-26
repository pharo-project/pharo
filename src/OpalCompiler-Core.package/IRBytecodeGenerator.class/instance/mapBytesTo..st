mapping
mapBytesTo: instr
	"Record the current byte offset in instruction sequence as start of instr. 
	This is later used to calculate the total byte offset of instruction in generated code,
	see #bytecodes"
	instrMap add: instr -> (bytes size + 1)