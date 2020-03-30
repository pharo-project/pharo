"
I am a completion context that tells a CoStatisticsCompletionEntry its index before being activated.
This is required because completion entries could be shown in different positions of the candidates, depending on the filters.
"
Class {
	#name : #CoStatisticsCompletionContext,
	#superclass : #CoCompletionContext,
	#category : #'HeuristicCompletion-Model-Statistics'
}

{ #category : #activating }
CoStatisticsCompletionContext >> activateEntryAt: anIndex [

	^ (self entries at: anIndex)
		index: anIndex;
		activateOn: self
]
