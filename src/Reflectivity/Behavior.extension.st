Extension { #name : 'Behavior' }

{ #category : '*Reflectivity' }
Behavior >> link: aMetaLink toTemporaryNamed: aTempVarName inMethod: aMethodName option: option [
	| variable |

	variable := (self lookupSelector: aMethodName) lookupVar: aTempVarName.
	aMetaLink
		installOnVariable: variable
		for:  self nonAnonymousClass
		option: option
		instanceSpecific: false
]

{ #category : '*Reflectivity' }
Behavior >> metaLinkOptions [
	^{
	#methodDict -> #( + optionCompileOnLinkInstallation).
	#metaLinkOptions -> #( + optionCompileOnLinkInstallation)
	}
]
