accessing
environmentForClassVariable: aSymbol in: aClass 
	| selectorEnvironment assoc block |
	selectorEnvironment := RBSelectorEnvironment onEnvironment: self.
	selectorEnvironment addSearchString: aSymbol.
	((classVariables at: aClass name ifAbsent: [#()]) includes: aSymbol) 
		ifFalse: [^selectorEnvironment].
	assoc := aClass bindingOf: aSymbol.
	block := 
			[:each | 
			(each whichSelectorsReferTo: assoc) 
				do: [:sel | selectorEnvironment addClass: each selector: sel]].
	aClass withAllSubAndSuperclassesDo: 
			[:each | 
			block
				value: each;
				value: each class].
	^selectorEnvironment