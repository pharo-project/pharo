"
SmalllintManifestChecker is responsible for running a set of rules on a given subsystem.

| rules checker | 
rules := RBCompositeLintRule allGoodRules resetResult.

""the resetResult is necessary because it will build a knid of cache of the result
and this cannot be done automatically (for example you may want to run several rules without invalidating the results). ""

checker := SmalllintManifestChecker new
	runRules: rules onPackage: (RPackageOrganizer default packageNamed: #'Manifest-Core').
"
Class {
	#name : #SmalllintManifestChecker,
	#superclass : #RBSmalllintChecker,
	#instVars : [
		'manifestClassCache'
	],
	#category : #'Manifest-Core-Base'
}

{ #category : #accessing }
SmalllintManifestChecker >> builderManifestClass [
	^ TheManifestBuilder
]

{ #category : #manifest }
SmalllintManifestChecker >> cleanAllManifest [
	| manifests |
	manifests := environment packages collect: [ :cl | self builderManifestClass of: cl ] as: Set.
	manifests do: [ :each | each cleanUp ]
]

{ #category : #accessing }
SmalllintManifestChecker >> criticsOf: aRule [

	^ rule leaves
		detect: [ :r | r class = aRule class ]
		ifFound: [ :rules | rules critics ]
		ifNone: [ #() ]
]

{ #category : #manifest }
SmalllintManifestChecker >> falsePositiveOf: aRule [

	| critics rId rV mb |

	critics := self criticsOf: aRule.
	rId := aRule class uniqueIdentifierName.
	rV := aRule class identifierMinorVersionNumber.
	^ critics
		select: [ :critic | 
			mb := self manifestBuilderOf: critic.
			mb ifNil: [ false ] ifNotNil: [ mb isFalsePositive: critic onRule: rId version: rV ]
			]
]

{ #category : #initialization }
SmalllintManifestChecker >> initialize [

	super initialize.
	manifestClassCache := Dictionary new.

]

{ #category : #manifest }
SmalllintManifestChecker >> isFalsePositive: aCritic forRuleId: ruleId versionId: versionId [

	| mb |

	mb := self manifestBuilderOf: aCritic.
	^ mb ifNil: [ false ] ifNotNil: [ mb isFalsePositive: aCritic onRule: ruleId version: versionId ]
]

{ #category : #manifest }
SmalllintManifestChecker >> isToDo: aCritic forRuleId: ruleId versionId: versionId [

	| mb |

	mb := self manifestBuilderOf: aCritic.
	^ mb ifNil: [ false ] ifNotNil: [ mb containsToDo: aCritic onRule: ruleId version: versionId ]
]

{ #category : #manifest }
SmalllintManifestChecker >> isTruePositive: aCritic forRuleId: ruleId versionId: versionId [ 
	
	^ (self manifestBuilderOf: aCritic)
			containsTruePositive: aCritic onRule:  ruleId version: versionId

]

{ #category : #accessing }
SmalllintManifestChecker >> manifestBuilderOf: aPackage [

	^ aPackage manifestBuilderForRuleChecker: self
	
]

{ #category : #accessing }
SmalllintManifestChecker >> manifestBuilderOfClass: aClass [
	| key |
	key := aClass instanceSide category.
	^ manifestClassCache
		at: key
		ifAbsentPut: [ 
			(self builderManifestClass hasManifestFor: aClass)
				ifTrue: [ self builderManifestClass of: aClass ]
				ifFalse: [ nil ] ]
]

{ #category : #accessing }
SmalllintManifestChecker >> manifestBuilderOfMethod: aMethod [
	| key cl |
	cl := aMethod methodClass.
	key := cl name , aMethod category asString.
	^ manifestClassCache
		at: key
		ifAbsentPut: [ 
			(self builderManifestClass hasManifestFor: aMethod)
				ifTrue: [ self builderManifestClass of: aMethod ]
				ifFalse: [ nil ] ]
]

{ #category : #accessing }
SmalllintManifestChecker >> manifestBuilderOfPackage: aPackage [
	| key |
	key := aPackage packageName.
	^ manifestClassCache
		at: key
		ifAbsentPut: [ 
			(self builderManifestClass hasPackageNamed: key)
				ifTrue: [ self builderManifestClass ofPackageNamed: key ]
				ifFalse: [ nil ] ]
]

{ #category : #manifest }
SmalllintManifestChecker >> rejectClassesOf: aPackage [

	| mb |

	mb := self manifestBuilderOf: aPackage.
	^ mb ifNil: [ {} ] ifNotNil: [ mb rejectClasses ]
]

{ #category : #manifest }
SmalllintManifestChecker >> rejectRulesOf: aPackage [

	| mb |

	mb := self manifestBuilderOf: aPackage.
	^ mb ifNil: [ {} ] ifNotNil: [ mb rejectRules ]
]

{ #category : #accessing }
SmalllintManifestChecker >> rule [
	^ rule
]

{ #category : #actions }
SmalllintManifestChecker >> runRules: aCompositeRule onPackage: aPackage [

	rule := aCompositeRule.
	environment := RBPackageEnvironment package: aPackage.
	self run.
]

{ #category : #actions }
SmalllintManifestChecker >> runRules: aCompositeRule onPackage: aPackage withoutTestCase: avoidTestCases [
	rule := aCompositeRule.
	environment := RBPackageEnvironment package: aPackage.
	
	environment := environment withoutClasses: (environment classes select: #isManifest).
	avoidTestCases 
		ifTrue: [ environment := environment withoutClasses: (environment classes select: #isTestCase) ].
	self run
]

{ #category : #manifest }
SmalllintManifestChecker >> toDoOf: aRule [

	| critics rId rV mb |

	critics := self criticsOf: aRule.
	rId := aRule class uniqueIdentifierName.
	rV := aRule class identifierMinorVersionNumber.
	^ critics
		select: [ :critic | 
			mb := self manifestBuilderOf: critic.
			mb ifNil: [ false ] ifNotNil: [ mb containsToDo: critic onRule: rId version: rV ]
			]
]
