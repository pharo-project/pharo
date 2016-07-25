private
addSlotInitToInitialize: aClass
	|  source |
	
	((aClass>>#initialize) hasLiteral: #initializeSlots:)ifTrue: [^self ].
	
	source := (aClass>>#initialize) sourceCode.
	(source endsWith: '.') ifFalse: [ source := source, '.' ].
	source := source, '
	self class initializeSlots: self.'.
	Author useAuthor: 'Generated' during: [aClass compile: source classified: 'initialization']. 
	