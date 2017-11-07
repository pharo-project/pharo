"
A rule to check for multiple periods terminating the same statement, which is unnecessary, probably unintentional, and can cause problems when porting to other platforms like GemStone.
"
Class {
	#name : #RBMultiplePeriodsTerminatingStatementRule,
	#superclass : #ReAbstractRule,
	#category : 'GeneralRules-Migrated'
}

{ #category : #testing }
RBMultiplePeriodsTerminatingStatementRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
RBMultiplePeriodsTerminatingStatementRule class >> uniqueIdentifierName [

	^ 'MultiplePeriodsTerminatingStatementRule'
]

{ #category : #running }
RBMultiplePeriodsTerminatingStatementRule >> check: aMethod forCritiquesDo: aCriticBlock [
	
	aMethod ast
		nodesDo: [ :each | 
			(each isSequence and:
			[ each periods size > each statements size ])
				ifTrue: [ aCriticBlock cull: (self critiqueFor: aMethod) ] ]
]

{ #category : #accessing }
RBMultiplePeriodsTerminatingStatementRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBMultiplePeriodsTerminatingStatementRule >> name [
	^ 'Multiple periods terminating the same statement'
]
