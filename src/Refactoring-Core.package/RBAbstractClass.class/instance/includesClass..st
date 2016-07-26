testing
includesClass: aRBClass 
	| currentClass |
	currentClass := self.
	[currentClass notNil and: [currentClass ~= aRBClass]] 
		whileTrue: [currentClass := currentClass superclass].
	^currentClass = aRBClass