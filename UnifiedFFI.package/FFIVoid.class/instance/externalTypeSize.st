accessing
externalTypeSize 
	self pointerArity < 1 ifTrue: [ ^ self error: 'void has unknown size' ].
	^ self pointerSize 