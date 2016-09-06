utilities
correspondingTestedMethodsOf: aMethodOrMethodDefinition
	| selectorString classTested methodsTested |
	aMethodOrMethodDefinition isTestMethod
		ifFalse: [ ^ #() ].

	"isTestMethod treats unary TestCase selectors starting with 'should'
	as test methods, so this check is needed to ignore them"
	((selectorString := aMethodOrMethodDefinition selector normalizedSelectorForComparison)
		beginsWith: 'test')
		ifFalse: [ ^ #() ].
	selectorString := selectorString allButFirst: 4.	"'test' size"

	"collect both instance and class corresponding methods"
	classTested := aMethodOrMethodDefinition methodClass correspondingClassTested
		ifNil: [ ^ #() ].
	methodsTested := OrderedCollection new.
	{classTested.
	classTested class}
		do: [ :classOrMetaclassTested | 
			classOrMetaclassTested
				selectorsDo: [ :each | 
					each normalizedSelectorForComparison = selectorString
						ifTrue: [ methodsTested addLast: classOrMetaclassTested >> each ] ] ].
	^ methodsTested asArray