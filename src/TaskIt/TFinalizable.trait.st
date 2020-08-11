Trait {
	#name : #TFinalizable,
	#category : #'TaskIt-Kernel'
}

{ #category : #starting }
TFinalizable >> noteDoesNotNeedToBeFinalized [
	WeakRegistry default remove: self ifAbsent: [  ]
]

{ #category : #starting }
TFinalizable >> noteNeedsToBeFinalized [
	WeakRegistry default add: self
]
