"
I am a completion engine that logs when the menu is closed without activating an entry.
"
Class {
	#name : #CoStatisticsCompletionEngine,
	#superclass : #CoCompletionEngine,
	#instVars : [
		'statistics'
	],
	#category : #'HeuristicCompletion-Statistics'
}

{ #category : #initialization }
CoStatisticsCompletionEngine >> closeMenu [

	context ifNotNil: [
		statistics logCancelEvent: (CoCompletionCancelledEvent new
			typedToken: context completionToken;
			contextClassName: (self completionClass ifNotNil: [ :class | class name ]);
			isDynamicContext: self isScripting;
			yourself) ].
	super closeMenu
]

{ #category : #initialization }
CoStatisticsCompletionEngine >> initialize [

	super initialize.
	statistics := CoStatistics uniqueInstance.
	completionBuilder := CoStatisticsASTHeuristicsResultSetBuilder new
		statistics: statistics;
		yourself.
	contextClass := CoStatisticsCompletionContext.
]
