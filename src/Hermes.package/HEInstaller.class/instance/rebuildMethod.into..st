installing methods
rebuildMethod: aMethod into: aClass
	| newMethod literalSpace extendedEnvironment |

	extendedEnvironment := HEExtendedEnvironment new.
	extendedEnvironment inner: environment.
	extendedEnvironment newClass: aClass.
	extendedEnvironment newSelector: aMethod name.

	newMethod := CompiledMethod newMethod: aMethod bytecode size header: (aMethod headerFor: extendedEnvironment).

	extendedEnvironment newMethod: newMethod.

	aMethod literals
		doWithIndex:
			[ :literal :idx | newMethod literalAt: idx put: (literal asLiteralIn: extendedEnvironment) ].

	newMethod classBinding: aClass binding.
	literalSpace := (aMethod literals size + 1) * Smalltalk wordSize.

	aMethod bytecode doWithIndex: [ :e :idx | newMethod at: idx + literalSpace put: e ].

	aClass
		addAndClassifySelector: aMethod name
		withMethod: newMethod
		inProtocol: aMethod protocol
		notifying: nil