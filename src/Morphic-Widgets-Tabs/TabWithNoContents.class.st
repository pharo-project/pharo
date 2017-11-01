"
Error raised when a tab is asked its contents but have no morph and no retrieving block, or the retrieving block is returning nil.
"
Class {
	#name : #TabWithNoContents,
	#superclass : #Error,
	#instVars : [
		'tab'
	],
	#category : 'Morphic-Widgets-Tabs-Support'
}

{ #category : #'instance creation' }
TabWithNoContents class >> signalTab: aTab [ 
	
	^ self new
		tab: aTab;
		signal
]

{ #category : #accessing }
TabWithNoContents >> tab [
	^tab
]

{ #category : #accessing }
TabWithNoContents >> tab: aTab [ 

	tab := aTab
]
