"
I am a completion engine that logs when the menu is closed without activating an entry.
"
Class {
	#name : 'CoStatisticsCompletionEngine',
	#superclass : 'CoCompletionEngine',
	#instVars : [
		'statistics'
	],
	#category : 'HeuristicCompletion-Morphic-Statistics',
	#package : 'HeuristicCompletion-Morphic',
	#tag : 'Statistics'
}

{ #category : 'menu morph' }
CoStatisticsCompletionEngine >> closeMenu [

	context ifNotNil: [
		statistics logCancelEvent: (CoCompletionCancelledEvent new
			typedToken: context completionToken;
			contextClassName: (self contextClass
					ifNotNil: [ :aClass | aClass name ]);
			isDynamicContext: self isScripting;
			yourself) ].
	super closeMenu
]

{ #category : 'initialization' }
CoStatisticsCompletionEngine >> initialize [

	super initialize.
	statistics := CoStatistics uniqueInstance.
	completionBuilder := CoStatisticsASTHeuristicsResultSetBuilder new
		statistics: statistics;
		yourself.
	contextClass := CoStatisticsCompletionContext
]
