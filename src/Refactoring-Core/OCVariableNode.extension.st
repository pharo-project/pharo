Extension { #name : 'OCVariableNode' }

{ #category : '*Refactoring-Core' }
OCVariableNode >> references: aVariableName [
	"Return wether the variable named aVariable is accessed in read or write with the receiver."
	
	^ self name = aVariableName
]
