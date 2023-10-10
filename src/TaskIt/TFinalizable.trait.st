Trait {
	#name : 'TFinalizable',
	#category : 'TaskIt-Kernel',
	#package : 'TaskIt',
	#tag : 'Kernel'
}

{ #category : 'starting' }
TFinalizable >> noteDoesNotNeedToBeFinalized [

	FinalizationRegistry default remove: self ifAbsent: [  ]
]

{ #category : 'starting' }
TFinalizable >> noteNeedsToBeFinalized [
	FinalizationRegistry default add: self
]
