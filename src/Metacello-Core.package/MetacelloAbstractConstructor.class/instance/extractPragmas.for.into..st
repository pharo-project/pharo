private
extractPragmas: pragmaKeyword for: aClass into: versionDict

	| versionString  pragmas |
	(Pragma 
		allNamed: pragmaKeyword
		in: aClass) do: [:pragma |
			versionString := pragma argumentAt: 1.
			pragmas := versionDict 
				at: versionString 
				ifAbsent: [ | list |
					list := OrderedCollection new.
					versionDict at: versionString put: list.
					list ].
			pragmas add: pragma ].