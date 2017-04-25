private
defineFieldOffset: fieldName value: offset
	"Add offset values to classPool."
	| offsetVarName |
	offsetVarName := self offsetVariableNameFor: fieldName.
	"Is this field defined in a superclass?"
	(superclass bindingOf: offsetVarName)
		ifNil: [
			(self hasClassVarNamed: offsetVarName) 
				ifFalse: [ self addClassVarNamed: offsetVarName ].
			self classVarNamed: offsetVarName put: offset ].