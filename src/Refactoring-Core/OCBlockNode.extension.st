Extension { #name : 'OCBlockNode' }

{ #category : '*Refactoring-Core' }
OCBlockNode >> references: aVariableName [
	"Return wether the variable named aVariable is accessed in read or write with the receiver."
	
	^ body references: aVariableName
]
