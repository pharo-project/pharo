"
A rule to check for a period terminating the method signature, which is unnecessary, probably unintentional, and can cause problems when portin to other platforms like GemStone.
"
Class {
	#name : #ReMethodSignaturePeriodRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReMethodSignaturePeriodRule class >> checksMethod [
	^ true
]

{ #category : #running }
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

{ #category : #accessing }
ReMethodSignaturePeriodRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReMethodSignaturePeriodRule >> name [
	^ 'Method signature has terminating period'
]
