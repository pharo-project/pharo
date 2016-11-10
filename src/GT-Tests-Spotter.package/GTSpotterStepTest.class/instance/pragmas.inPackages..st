private-navigation
pragmas: aSelector inPackages: anArray
	| pragmas |
	pragmas := OrderedCollection new.
	RPackageOrganizer default packages
		do: [ :package | 
			anArray
				do: [ :packageName | 
					package packageName = packageName
						ifTrue: [ package methods
								do: [ :cm | 
									cm methodClass isTrait
										ifFalse: [ (cm pragmas detect: [ :p | p selector = aSelector ] ifNone: [ nil ])
												ifNotNil: [ :p | pragmas add: p ] ] ] ] ] ].
	^ pragmas