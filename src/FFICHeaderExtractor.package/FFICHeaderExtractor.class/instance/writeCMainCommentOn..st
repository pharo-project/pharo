generating C program
writeCMainCommentOn: stream
	"Internal method for the C program creation. This one just writes a general comment at the beginning."
	stream nextPutAll: ('//
//
//  Program automatically generated on <1s> by FFICHeaderExtractor.
// For more details, see https://github.com/marianopeck/FFICHeaderExtractor
//
//
'	expandMacrosWith: Date today yyyymmdd ).
	stream cr.