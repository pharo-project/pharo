"
A rule to check for multiple periods terminating the same statement, which is unnecessary, probably unintentional, and can cause problems when porting to other platforms like GemStone.
"
Class {
	#name : #RBMultiplePeriodsTerminatingStatementRule,
	#superclass : #RBBlockLintRule,
	#category : 'Refactoring-Critics-BlockRules'
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
RBMultiplePeriodsTerminatingStatementRule >> basicCheck: aMethod [
	aMethod parseTree
		nodesDo: [ :each | 
			(each isSequence and: [ each periods size > each statements size ])
				ifTrue: [ ^ true ] ].
	^ false
]

{ #category : #running }
RBMultiplePeriodsTerminatingStatementRule >> checkMethod: aMethod [
	(self basicCheck: aMethod)
		ifTrue: [ result addMethod: aMethod ]
]

{ #category : #accessing }
RBMultiplePeriodsTerminatingStatementRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBMultiplePeriodsTerminatingStatementRule >> name [
	^ 'Multiple periods terminating the same statement'
]
