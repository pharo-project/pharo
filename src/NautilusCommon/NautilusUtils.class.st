"
A class of utility methods used to implement *NautilusCommon extension methods on Kernel classes like CompiledMethod and their Ring equivalents like RGMethodDefinition.
"
Class {
	#name : #NautilusUtils,
	#superclass : #Object,
	#category : #NautilusCommon
}

{ #category : #utilities }
NautilusUtils class >> asTestMethod: aMethod [
	^ aMethod isTestMethod
		ifTrue: [ aMethod ]
		ifFalse: [ aMethod correspondingTestMethod ]
]

{ #category : #utilities }
NautilusUtils class >> correspondingTestCaseOf: aClassOrClassDefinition [
	^ self environment
		at:
			(aClassOrClassDefinition theNonMetaClass name asString , 'Test') asSymbol
		ifAbsent: [ nil ]
]

{ #category : #utilities }
NautilusUtils class >> correspondingTestMethodOf: aMethodOrMethodDefinition [
	| selectorString testCase |
	aMethodOrMethodDefinition isTestMethod
		ifTrue: [ ^ nil ].
	selectorString := 'test'
		, aMethodOrMethodDefinition selector normalizedSelectorForComparison.
	(testCase := aMethodOrMethodDefinition methodClass correspondingTestCase)
		ifNil: [ ^ nil ].
	^ testCase methods
		detect: [ :each | each selector normalizedSelectorForComparison = selectorString ]
		ifNone: [ nil ]
]

{ #category : #utilities }
NautilusUtils class >> correspondingTestedMethodsOf: aMethodOrMethodDefinition [
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
]
