private
generateNativeBoostCallErrorMethodFrom: aCompiledMethod
	| seq args |
	seq := RBSequenceNode
		statements:
			{RBMessageNode receiver: (RBVariableNode named: #CannotDecompileNativeBoostCalls) selector: #signal arguments: #()}.
	args := OrderedCollection new.
	1 to: aCompiledMethod numArgs do: [ :i | args add: (RBArgumentNode named: 'arg' , i asString) ].
	^ builder
		codeMethod: aCompiledMethod selector
		arguments: args
		body: seq
		pragmas: (self pragmasForMethod: aCompiledMethod)
		class: aCompiledMethod methodClass