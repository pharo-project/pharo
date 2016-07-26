private
copyFrom: aSourceClass to: aTargetClass
	aSourceClass instanceVariableNames
		do: [ :each | aTargetClass addInstanceVariable: each ].
	aSourceClass isMeta ifFalse: [
		aSourceClass allClassVariableNames
			do: [ :each | aTargetClass addClassVariable: each ].
		aSourceClass sharedPoolNames
			do: [ :each | aTargetClass addPoolDictionary: each ] ].
	aSourceClass selectors do: [ :each |
		aTargetClass
			compile: (aSourceClass sourceCodeFor: each)
			classified: (aSourceClass protocolsFor: each) ]