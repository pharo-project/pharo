"
A rule to check for a period terminating the method signature, which is unnecessary, probably unintentional, and can cause problems when portin to other platforms like GemStone.
"
Class {
	#name : 'ReMethodSignaturePeriodRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'testing' }
ReMethodSignaturePeriodRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReMethodSignaturePeriodRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReMethodSignaturePeriodRule class >> ruleName [
	^ 'Method signature has terminating period'
]

{ #category : 'running' }
ReMethodSignaturePeriodRule >> check: aMethod forCritiquesDo: aCriticBlock [

	| firstLine hasDot |
	firstLine := aMethod sourceCode lines first.
	hasDot := firstLine trimRight last == $..
	hasDot ifTrue: [
		aCriticBlock cull: (ReTrivialCritique
				 withAnchor: (ReIntervalSourceAnchor
						  entity: aMethod
						  interval: (1 to: firstLine size))
				 by: self) ]
]
