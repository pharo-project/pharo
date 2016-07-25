private
displayClass: aClassName instanceVariable: aVariableName
	"^ aClassName, '/slots #', aVariableName"
	"^ aClassName, ':', aVariableName"
	"^ aClassName, ' slot: #', aVariableName"
	^ (self displayBehavior: aClassName), (self displayOperation: 'instanceVariableNamed:'), (self displayString: '#', aVariableName)