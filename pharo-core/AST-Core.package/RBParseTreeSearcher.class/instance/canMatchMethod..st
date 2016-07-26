testing
canMatchMethod: aCompiledMethod
	| actualMessages |
	self messages isEmpty
		ifTrue: [ ^ true ].
	actualMessages := aCompiledMethod messages.
	^ self messages 
		anySatisfy: [ :each | actualMessages includes: each ]