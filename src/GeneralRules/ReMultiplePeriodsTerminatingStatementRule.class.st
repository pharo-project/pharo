"
A rule to check for multiple periods terminating the same statement, which is unnecessary, probably unintentional, and can cause problems when porting to other platforms like GemStone.
"
Class {
	#name : #ReMultiplePeriodsTerminatingStatementRule,
	#superclass : #ReAbstractRule,
	#category : #'GeneralRules-Migrated'
}

{ #category : #testing }
ReMultiplePeriodsTerminatingStatementRule class >> checksMethod [
	^ true
]

{ #category : #accessing }
ReMultiplePeriodsTerminatingStatementRule class >> uniqueIdentifierName [

	^ 'MultiplePeriodsTerminatingStatementRule'
]

{ #category : #running }
ReMultiplePeriodsTerminatingStatementRule >> check: aMethod forCritiquesDo: aCriticBlock [
	
	aMethod ast
		nodesDo: [ :node | 
			(node isSequence and:
			[ node periods size > node statements size ])
				ifTrue: [ 
					self periodPairs: node critiqueBlock: aCriticBlock in: aMethod ] ]
]

{ #category : #accessing }
ReMultiplePeriodsTerminatingStatementRule >> group [
	^ 'Potential Bugs'
]

{ #category : #accessing }
ReMultiplePeriodsTerminatingStatementRule >> name [
	^ 'Multiple periods terminating the same statement'
]

{ #category : #running }
ReMultiplePeriodsTerminatingStatementRule >> periodPairs: node critiqueBlock: aCriticBlock in: aMethod [
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
