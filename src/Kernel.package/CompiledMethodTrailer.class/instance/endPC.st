accessing
endPC
	"Answer the index of the last bytecode."
	
	method ifNil: [ self error: 'Cannot determine the endPC without compiled method' ].
	
	"if method set, then size should be set as well"
	^ method size - size