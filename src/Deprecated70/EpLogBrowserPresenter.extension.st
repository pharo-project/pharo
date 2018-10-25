Extension { #name : #EpLogBrowserPresenter }

{ #category : #'*Deprecated70' }
EpLogBrowserPresenter >> entryContentModel [
	self deprecated: 'Use #entryContentPresenter instead' transformWith: '`@receiver entryContentModel' -> '`@receiver entryContentPresenter'.
	^ self entryContentPresenter 
]

{ #category : #'*Deprecated70' }
EpLogBrowserPresenter >> initializeEntryContentModel [
	
	self deprecated: 'Use #initializeEntryContentPresenter instead' transformWith: '`@receiver initializeEntryContentModel' -> '`@receiver initializeEntryContentPresenter'.
	^ self initializeEntryContentPresenter
]

{ #category : #'*Deprecated70' }
EpLogBrowserPresenter >> initializeItemsModel [
	
	self deprecated: 'Use #initializeItemsPresenter instead' transformWith: '`@receiver initializeItemsModel' -> '`@receiver initializeItemsPresenter'.
	^ self initializeItemsPresenter
]

{ #category : #'*Deprecated70' }
EpLogBrowserPresenter >> initializeToolbarModel [
	
	self deprecated: 'Use #initializeToolbarPresenter instead' transformWith: '`@receiver initializeToolbarModel' -> '`@receiver initializeToolbarPresenter'.
	^ self initializeToolbarPresenter
]

{ #category : #'*Deprecated70' }
EpLogBrowserPresenter >> itemsModel [
	
	self deprecated: 'Use #itemsPresenter instead' transformWith: '`@receiver itemsModel' -> '`@receiver itemsPresenter'.
	^ self itemsPresenter
]

{ #category : #'*Deprecated70' }
EpLogBrowserPresenter >> refreshEntryContentModelWith: selectedItems [
	
	self
		deprecated: 'Please use #refreshEntryContentPresenterWith: instead'
		transformWith: '`@receiver refreshEntryContentModelWith: `@statements' 
						-> '`@receiver refreshEntryContentPresenterWith: `@statements'.
						
	^ self refreshEntryContentPresenterWith: selectedItems
]

{ #category : #'*Deprecated70' }
EpLogBrowserPresenter >> toolbarModel [
	
	self deprecated: 'Use #toolbarPresenter instead' transformWith: '`@receiver toolbarModel' -> '`@receiver toolbarPresenter'.
	^ self toolbarPresenter
]
