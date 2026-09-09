Extension { #name : 'OCProgramNode' }

{ #category : '*Refactoring-Core' }
OCProgramNode >> references: aVariableName [
	"Return wether the variable named aVariable is accessed in read or write with the receiver."

	^ self children anySatisfy: [:each | each references: aVariableName]
]
