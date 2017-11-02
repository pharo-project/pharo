Class {
	#name : #ReSmokeExceptionStrategyTest,
	#superclass : #ReExceptionStrategyBaseTest,
	#instVars : [
		'brokenRule'
	],
	#category : 'Renraku-Test'
}

{ #category : #running }
ReSmokeExceptionStrategyTest >> setUp [
	super setUp.

	brokenRule := ReAbstractRule
		subclass: #ReBrokenRuleForTesting
		instanceVariableNames: ''
		classVariableNames: ''
		package: RPackage defaultPackageName.
		
	brokenRule class
		compile: 'checksMethod ^ true'.
		
	brokenRule
		compile: 'name ^ ''A purpously broken rule to test exception handling''';
		compile: 'check: aNode forCritiquesDo: aBlock
						MyTestError signal: ',
						self errorMessage surroundedBySingleQuotes.
						
	ReRuleManager reset
]

{ #category : #running }
ReSmokeExceptionStrategyTest >> tearDown [

	brokenRule removeFromSystem.
	brokenRule := nil.
	super tearDown.
]

{ #category : #tests }
ReSmokeExceptionStrategyTest >> testIgnore [
	| critiques |

	ReExceptionStrategy current: ReIgnoreExceptionStrategy.
	
	self shouldnt: [
			critiques := thisContext method critiques ]
		raise: MyTestError.
]

{ #category : #tests }
ReSmokeExceptionStrategyTest >> testLog [
	| critiques method |
	method := thisContext method.

	ReExceptionStrategy current: ReLogExceptionStrategy.
	
	self shouldnt: [
		critiques := method critiques ]
		raise: MyTestError.
		
	self assert: (critiques anySatisfy: [ :crit | 
		(crit isKindOf: ReExceptionProperty) and: [
		 crit sourceAnchor entity = method and: [
		(crit message includesSubstring: self errorMessage) and: [
		 crit stack callChainAnySatisfy: [ :context |
			context method = method ] ] ] ] ])
]

{ #category : #tests }
ReSmokeExceptionStrategyTest >> testPass [

	ReExceptionStrategy current: RePassExceptionStrategy.
	
	self should: [ thisContext method critiques ]
		raise: MyTestError.
]
