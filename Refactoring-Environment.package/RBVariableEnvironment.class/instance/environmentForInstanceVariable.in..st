accessing
environmentForInstanceVariable: aString in: aClass 
	| selectorEnvironment isReader isWriter |
	selectorEnvironment := RBSelectorEnvironment onEnvironment: self.
	selectorEnvironment addSearchString: aString.
	isReader := isWriter := false.
	((instanceVariables at: aClass name ifAbsent: [#()]) includes: aString) 
		ifTrue: 
			[isReader := true.
			isWriter := true].
	((instanceVariableWriters at: aClass name ifAbsent: [#()]) 
		includes: aString) ifTrue: [isWriter := true].
	((instanceVariableReaders at: aClass name ifAbsent: [#()]) 
		includes: aString) ifTrue: [isReader := true].
	aClass withAllSubAndSuperclassesDo:
			[:each | 
			isWriter 
				ifTrue: 
					[(each whichSelectorsAssign: aString) 
						do: [:sel | selectorEnvironment addClass: each selector: sel]].
			isReader 
				ifTrue: 
					[(each whichSelectorsRead: aString) 
						do: [:sel | selectorEnvironment addClass: each selector: sel]]].
	^selectorEnvironment