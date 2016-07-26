accessing
linePosition: offset 
	"Answer the position, in same line as receiver, relative to line start.
	If offset bigger than line size, answer position for the end of line. "
	self subclassResponsibility