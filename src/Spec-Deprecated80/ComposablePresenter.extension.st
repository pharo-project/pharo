Extension { #name : #ComposablePresenter }

{ #category : #'*Spec-Deprecated80' }
ComposablePresenter >> defaultWindowModelClass [ 
	self deprecated: 'Use #defaultWindowPresenterClass instead' transformWith: '`@receiver defaultWindowModelClass' -> '`@receiver defaultWindowPresenterClass'.
	
	^ self defaultWindowPresenterClass
]

{ #category : #'*Spec-Deprecated80' }
ComposablePresenter >> icon: aSymbol [
	self deprecated: 'Use #iconNamed: instead' transformWith: '`@receiver icon: `@statements' -> '`@receiver iconNamed: `@statements'.
	^ self iconNamed: aSymbol
]

{ #category : #'*Spec-Deprecated80' }
ComposablePresenter >> instantiateModels: aCollectionOfPairs [
	
	self deprecated: 'Use #instantiatePresenters: instead' transformWith: '`@receiver instantiateModels: `@statement' -> '`@receiver instantiatePresenters: `@statement'.

	^ self instantiatePresenters: aCollectionOfPairs
]

{ #category : #'*Spec-Deprecated80' }
ComposablePresenter >> newTab [

	"
	Commented to allow Iceberg Migration.
	self 
		deprecated: 'Use newNotebook instead' 
		on: '2019-01-24' 
		in: #Pharo8."
	^ self instantiate: TabPresenter
]

{ #category : #'*Spec-Deprecated80' }
ComposablePresenter >> newTabManager [
	
	"
	Commented to allow Iceberg Migration.
	self 
		deprecated: 'Use newNotebook instead' 
		on: '2019-01-24' 
		in: #Pharo8."
	
	^ self instantiate: TabManagerPresenter
]
