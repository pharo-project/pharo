utils
changeClassesNames: aPackage
	aPackage instVarNamed: #packageName put: #'Hermes-Tests-External-Package2'.
	aPackage classes
		do: [ :e | 
			e instVarNamed: #category put: #'Hermes-Tests-External-Package2'.
			e instVarNamed: #className put: (e className , '2') asSymbol.
			self updateMethodLiterals: e methods.
			self updateMethodLiterals: e classSideMethods ].

	aPackage traits
		do: [ :e | 
			e instVarNamed: #category put: #'Hermes-Tests-External-Package2'.
			e instVarNamed: #traitName put: (e className , '2') asSymbol.
			self updateMethodLiterals: e methods.
			self updateMethodLiterals: e classSideMethods ].

	aPackage extensionMethods
		do: [ :e | 
			e instVarNamed: #name put: (e name , '2') asSymbol.
			e instVarNamed: #protocol put: #'*Hermes-Tests-External-Package2' ].
