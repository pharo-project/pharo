Extension { #name : #EpLogBrowserToolbarPresenter }

{ #category : #'*Deprecated70' }
EpLogBrowserToolbarPresenter >> filtersButtonModel [
	self deprecated: 'Use #filtersButtonPresenter instead' transformWith: '`@receiver filtersButtonModel' -> '`@receiver filtersButtonPresenter'.

	^ self filtersButtonPresenter
]

{ #category : #'*Deprecated70' }
EpLogBrowserToolbarPresenter >> logBrowserModel [
	self deprecated: 'Use #logBrowserPresenter instead' transformWith: '`@receiver logBrowserModel' -> '`@receiver logBrowserPresenter'.
	
	^ self logBrowserPresenter
]

{ #category : #'*Deprecated70' }
EpLogBrowserToolbarPresenter >> logBrowserModel: anObject [
	
	self
		deprecated: 'Please use #logBrowserPresenter: instead'
		transformWith: '`@receiver logBrowserModel: `@statements' 
						-> '`@receiver logBrowserPresenter: `@statements'.
						
	self logBrowserPresenter: anObject
]

{ #category : #'*Deprecated70' }
EpLogBrowserToolbarPresenter >> statusLabelModel [
	self deprecated: 'Use #statusLabelPresenter instead' transformWith: '`@receiver statusLabelModel' -> '`@receiver statusLabelPresenter'.

	^ self statusLabelPresenter
]
