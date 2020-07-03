"
SUnit tests for SmalllintManifestChecker
"
Class {
	#name : #SmalllintManifestCheckerTest,
	#superclass : #AbstractEnvironmentTestCase,
	#instVars : [
		'checker'
	],
	#category : #'Manifest-Tests-Base'
}

{ #category : #running }
SmalllintManifestCheckerTest >> cleaningResources [
	testingEnvironment
		at: #ManifestManifestResourcesTests
		ifPresent: [ :cl | 
			cl
				removeFromChanges;
				removeFromSystem ]
]

{ #category : #private }
SmalllintManifestCheckerTest >> package [ 
	MCWorkingCopy managersForClass: MFClassA  do: [:p | ^ p packageSet packages first].
	" should be equivalent to RPackageOrganizer default packageNamed: #'Manifest-Resources-Tests' "
]

{ #category : #running }
SmalllintManifestCheckerTest >> setUp [
	| bm |
	super setUp.
	self cleaningResources.
	bm := TheManifestBuilder of: MFClassA.
	bm installFalsePositiveOf: ReCodeCruftLeftInMethodsRule uniqueIdentifierName version: 1.
	bm addFalsePositive: MFClassB >> #method3 of: ReCodeCruftLeftInMethodsRule uniqueIdentifierName version: 1.
	bm installToDoOf: ReTemporaryNeitherReadNorWrittenRule uniqueIdentifierName version: 1.
	bm
		addAllToDo:
			{(MFClassB >> #method3).
			(MFClassA >> #method)}
		of: ReTemporaryNeitherReadNorWrittenRule uniqueIdentifierName
		version: 1.
	checker := ReSmalllintChecker new
]

{ #category : #running }
SmalllintManifestCheckerTest >> tearDown [
	
	self cleaningResources.
	ASTCache reset.
	super tearDown 
]

{ #category : #tests }
SmalllintManifestCheckerTest >> testCriticsOf [

	| rule critiques |
	rule := ReRuleManager visibleRuleClasses collect: [:cl | cl new].
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.
	critiques := checker criticsOf: ReTemporaryNeitherReadNorWrittenRule new.
	self assert: critiques size equals: 3.
	self assert: (critiques anySatisfy: [ :each | each sourceAnchor entity = (MFClassB >> #method3) ]).
	self assert: (critiques anySatisfy: [ :each | each sourceAnchor entity = (MFClassA >> #method) ])
]

{ #category : #tests }
SmalllintManifestCheckerTest >> testIsFalsePositive [ 

	| rule |
	rule  := ReRuleManager visibleRuleClasses collect: [:cl | cl new].
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.  	
	self assert: (checker isFalsePositive:  (MFClassB>>#method3) forRuleId: (ReCodeCruftLeftInMethodsRule uniqueIdentifierName) versionId:  1).
	self deny: (checker isFalsePositive:  (MFClassA>>#method) forRuleId: (ReCodeCruftLeftInMethodsRule uniqueIdentifierName) versionId:  1).


]

{ #category : #tests }
SmalllintManifestCheckerTest >> testIsToDo [

	| rule |
	rule  := ReRuleManager visibleRuleClasses collect: [ :each | each new ].
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.
	
	self assert: (checker isToDo:  (MFClassB>>#method3) forRuleId: (ReTemporaryNeitherReadNorWrittenRule uniqueIdentifierName) versionId:  1).
	self deny: (checker isToDo:  (MFClassB>>#method2) forRuleId: (ReTemporaryNeitherReadNorWrittenRule uniqueIdentifierName) versionId:  1).


]

{ #category : #tests }
SmalllintManifestCheckerTest >> testToDoOf [

	| rule |
	rule := ReRuleManager visibleRuleClasses collect: [:cl | cl new].
	checker
		rule: rule;
		environment: self package asEnvironment;
		run.
		
	self assert: (( checker toDoOf: ReTemporaryNeitherReadNorWrittenRule new) anySatisfy: [:each|
		each sourceAnchor entity = (MFClassB>>#method3)]).
	self deny: (( checker toDoOf: ReTemporaryNeitherReadNorWrittenRule new) anySatisfy: [:each|
		each  sourceAnchor entity = (MFClassB>>#method2)]).
]
