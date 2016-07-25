methods
tempsReadBeforeWritten
	| temp |
	self isVariable ifTrue: [temp := 4].
	^temp