Extension { #name : #EpUnifiedBrowserPresenter }

{ #category : #'*Deprecated70' }
EpUnifiedBrowserPresenter >> logModel [
	self deprecated: 'Use #logPresenter instead' transformWith: '`@receiver logModel' -> '`@receiver logPresenter'.
	^ self logPresenter
]

{ #category : #'*Deprecated70' }
EpUnifiedBrowserPresenter >> logNodesModel [
	self deprecated: 'Use #logNodesPresenter instead' transformWith: '`@receiver logNodesModel' -> '`@receiver logNodesPresenter'.
	^ self logNodesPresenter
]
