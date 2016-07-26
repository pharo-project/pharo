operations
addStyle: aStyle
	"Register a new style, but only if none of my existing styles match the argument; answer the registered style"
	
	aStyle = defaultStyle ifTrue: [ ^ defaultStyle ].
	
	^ styles like: aStyle ifAbsent: [ styles add: aStyle ]
	
	