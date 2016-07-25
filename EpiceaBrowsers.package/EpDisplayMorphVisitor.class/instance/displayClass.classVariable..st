private
displayClass: aClassName classVariable: aVariableName
	"^ aClassName, '/slots #', aVariableName"
	"^ aClassName, ':', aVariableName"
	"^ aClassName, ' classSlot: #', aVariableName"
	^ (self displayBehavior: aClassName), (self displayOperation: 'classVariableNamed:'), (self displayString: '#', aVariableName)