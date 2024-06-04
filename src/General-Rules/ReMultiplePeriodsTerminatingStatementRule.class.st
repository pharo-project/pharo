"
A rule to check for multiple periods terminating the same statement, which is unnecessary, probably unintentional, and can cause problems when porting to other platforms like GemStone.
"
Class {
	#name : 'ReMultiplePeriodsTerminatingStatementRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Potential Bugs',
	#package : 'General-Rules',
	#tag : 'Potential Bugs'
}

{ #category : 'testing' }
ReMultiplePeriodsTerminatingStatementRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReMultiplePeriodsTerminatingStatementRule class >> group [
	^ 'Potential Bugs'
]

{ #category : 'accessing' }
ReMultiplePeriodsTerminatingStatementRule class >> ruleName [
	^ 'Multiple periods terminating the same statement'
]

{ #category : 'accessing' }
ReMultiplePeriodsTerminatingStatementRule class >> uniqueIdentifierName [

	^ 'MultiplePeriodsTerminatingStatementRule'
]

{ #category : 'running' }
ReMultiplePeriodsTerminatingStatementRule >> check: aMethod forCritiquesDo: aCriticBlock [

	aMethod ast nodesDo: [ :node |
		node isSequence and: [
			self periodPairs: node critiqueBlock: aCriticBlock in: aMethod ] ]
]

{ #category : 'running' }
ReMultiplePeriodsTerminatingStatementRule >> periodPairs: node critiqueBlock: aCriticBlock in: aMethod [

	| periods critique |
	periods := node periods.
	2 to: periods size do: [ :index | | code start stop trimmed |
		start := periods at: index - 1.
		stop := periods at: index.
		code := node methodNode sourceCode copyFrom: start + 1 to: stop - 1.
		trimmed := code trim.
		(trimmed isEmpty or: [ trimmed first = $" and: [ trimmed last = $" ] ]) ifTrue: [
			critique := ReTrivialCritique 
					 withAnchor: (ReIntervalSourceAnchor entity: aMethod interval: (start to: stop))
					 by: self
					 hint: 'two periods'.
			aCriticBlock cull: critique ] ]
]
