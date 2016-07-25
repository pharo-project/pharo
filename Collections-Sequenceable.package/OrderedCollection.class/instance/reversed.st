copying
reversed
	"Answer a copy of the receiver with element order reversed.  "
	| newCol |
	newCol := self species new.
	self reverseDo:
		[:elem | newCol addLast: elem].
	^ newCol

"#(2 3 4 'fred') reversed"