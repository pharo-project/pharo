"
A rule to check for multiple periods terminating the same statement, which is unnecessary, probably unintentional, and can cause problems when porting to other platforms like GemStone.
"
Class {
	#name : #RBMultiplePeriodsTerminatingStatementRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
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
		nodesDo: [ :node | 
			(node isSequence and:
			[ node periods size > node statements size ])
				ifTrue: [ 
					self periodPairs: node critiqueBlock: aCriticBlock in: aMethod ] ]
]

{ #category : #accessing }
RBMultiplePeriodsTerminatingStatementRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
RBMultiplePeriodsTerminatingStatementRule >> name [
	^ 'Multiple periods terminating the same statement'
]

{ #category : #running }
RBMultiplePeriodsTerminatingStatementRule >> periodPairs: node critiqueBlock: aCriticBlock in: aMethod [
	| periods |
	periods := node periods.
	2 to: periods size do: [ :index | |code start stop |
		start := periods at: index-1.
		stop := periods at: index.
		code := node methodNode sourceCode copyFrom: start+1 to: stop-1.
		code trim ifEmpty: [ 
			aCriticBlock cull: (ReTrivialCritique  
			withAnchor: (ReIntervalSourceAnchor
				entity: aMethod
				interval: (start to: stop))
			by: self
			hint: 'two periods') ] ]
	
]
