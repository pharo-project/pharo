access
sourceCode
	
	self type= #preamble ifTrue: [ ^ #preamble printString ].
	self type= #doIt ifTrue: [ ^ #preamble printString ].
	self type= #classComment ifTrue: [ self commentClass ifNotNil: [ :comment | ^ comment comment ]].

	^ self string