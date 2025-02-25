"
SUnit tests for SmalllintManifestChecker
"
Class {
	#name : 'SmalllintManifestCheckerTest',
	#superclass : 'AbstractEnvironmentTestCase',
	#instVars : [
		'checker'
	],
	#category : 'Manifest-Tests-Base',
	#package : 'Manifest-Tests',
	#tag : 'Base'
}

{ #category : 'running' }
SmalllintManifestCheckerTest >> setUp [

	super setUp.

	(TheManifestBuilder of: MFClassA)
		installFalsePositiveOf: ReCodeCruftLeftInMethodsRule uniqueIdentifierName version: 1;
		addFalsePositive: MFClassB >> #method3 of: ReCodeCruftLeftInMethodsRule uniqueIdentifierName version: 1;
		installToDoOf: ReTemporaryNeitherReadNorWrittenRule uniqueIdentifierName version: 1;
		addAllToDo: {
				(MFClassB >> #method3).
				(MFClassA >> #method) }
		of: ReTemporaryNeitherReadNorWrittenRule uniqueIdentifierName version: 1.

	checker := ReSmalllintChecker new.
	checker
		rule: (ReRuleManager visibleRuleClasses collect: [ :class | class new ]);
		environment: MFClassA package asEnvironment;
		run
]

{ #category : 'running' }
SmalllintManifestCheckerTest >> tearDown [

	testingEnvironment at: #ManifestManifestResourcesTests ifPresent: [ :class | class removeFromSystem ].
	super tearDown
]

{ #category : 'tests' }
SmalllintManifestCheckerTest >> testCriticsOf [

	| critiques |
	critiques := checker criticsOf: ReTemporaryNeitherReadNorWrittenRule new.
	self assert: critiques size equals: 3.
	self assert: (critiques anySatisfy: [ :each | each sourceAnchor entity = (MFClassB >> #method3) ]).
	self assert: (critiques anySatisfy: [ :each | each sourceAnchor entity = (MFClassA >> #method) ])
]

{ #category : 'tests' }
SmalllintManifestCheckerTest >> testIsFalsePositive [

	self assert: (checker isFalsePositive: MFClassB >> #method3 forRuleId: ReCodeCruftLeftInMethodsRule uniqueIdentifierName versionId: 1).
	self deny: (checker isFalsePositive: MFClassA >> #method forRuleId: ReCodeCruftLeftInMethodsRule uniqueIdentifierName versionId: 1)
]

{ #category : 'tests' }
SmalllintManifestCheckerTest >> testIsToDo [

	self assert: (checker isToDo: MFClassB >> #method3 forRuleId: ReTemporaryNeitherReadNorWrittenRule uniqueIdentifierName versionId: 1).
	self deny: (checker isToDo: MFClassB >> #method2 forRuleId: ReTemporaryNeitherReadNorWrittenRule uniqueIdentifierName versionId: 1)
]

{ #category : 'tests' }
SmalllintManifestCheckerTest >> testToDoOf [

	self assert: ((checker toDoOf: ReTemporaryNeitherReadNorWrittenRule new) anySatisfy: [ :each | each sourceAnchor entity = (MFClassB >> #method3) ]).
	self deny: ((checker toDoOf: ReTemporaryNeitherReadNorWrittenRule new) anySatisfy: [ :each | each sourceAnchor entity = (MFClassB >> #method2) ])
]
