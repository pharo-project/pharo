Extension { #name : 'OCArrayNode' }

{ #category : '*Refactoring-Core' }
OCArrayNode >> references: aVariableName [
	"Return wether the variable named aVariable is accessed in read or write with the receiver."
		
	^ statements anySatisfy: [ :each | each references: aVariableName ]
]
