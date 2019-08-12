Extension { #name : #ComposablePresenter }

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> beginsWith: aString fromList: aMorph [
	"This method is used bu the list for the search of elements when you are typing directly in the list"
	
	^ self ensureMorphicAdapterDo: [ :adapter | adapter beginsWith: aString fromList: aMorph ]
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> detectMorphicAdapterDo: aBlock [

	^ self dependents 
		detect: [:e | e isMorphicAdapter ]
		ifFound: aBlock
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> detectMorphicAdapterDo: doBlock ifNone: aBlock [

	^ self dependents 
		detect: [:e | e isMorphicAdapter ]
		ifFound: doBlock
		ifNone: aBlock
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> ensureMorphicAdapter [

	^ self dependents 
		detect: [:e | e isMorphicAdapter ]
		ifNone: [ self buildWithSpec ]
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> ensureMorphicAdapterDo: aBlock [

	^ aBlock value: self ensureMorphicAdapter
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> heightToDisplayInList: aList [
	"Return the width of my representation as a list item"
	
	^ self ensureMorphicAdapterDo: [ :adapter | adapter heightToDisplayInList: aList ]
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> heightToDisplayInTree: aTree [
	"Return the width of my representation as a list item"
	
	^ self ensureMorphicAdapterDo: [ :adapter | adapter heightToDisplayInTree: aTree ]
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> listRenderOn: aCanvas atRow: aRow bounds: drawBounds color: drawColor backgroundColor: backgroundColor from: aMorph [
	"Specify how this object as a list item should be drawn"

	^ self ensureMorphicAdapterDo: [ :adapter | adapter
		listRenderOn: aCanvas
		atRow: aRow
		bounds: drawBounds
		color: drawColor
		backgroundColor: backgroundColor
		from: aMorph ]
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> openWorldWithSpec [
	"Build the widget using the default spec and display it into the world"

	^ self openWorldWithSpec: self defaultSpecSelector.
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> openWorldWithSpec: aSpec [
	"Build the widget using the spec name provided as argument and display it into the world"

	^ self openWorldWithSpecLayout: (self retrieveSpec: aSpec)
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> openWorldWithSpecLayout: aSpec [
	"Build the widget using the spec name provided as argument and display it into the world"

	(window value notNil and: [ self needRebuild not ])
		ifTrue: [
			window value rebuildWithSpecLayout: aSpec ]
		ifFalse: [ 
			window value: (WorldPresenter new model: self).
			window value openWithSpecLayout: aSpec.
			self takeKeyboardFocus ].
		
	^ window value
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> treeRenderOn: aCanvas bounds: drawBounds color: drawColor font: aFont from: aMorph [
	"Specify how this object as a list item should be drawn"

	^ self ensureMorphicAdapterDo: [ :adapter | adapter 
		treeRenderOn: aCanvas 
		bounds: drawBounds 
		color: drawColor 
		font: aFont 
		from: aMorph ]
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> widthToDisplayInList: aList [
	"Return the width of my representation as a list item"
	
	^ self ensureMorphicAdapterDo: [ :adapter | adapter widthToDisplayInList: aList ]
]

{ #category : #'*Spec-MorphicAdapters' }
ComposablePresenter >> widthToDisplayInTree: aTree [
	"Return the width of my representation as a list item"
	
	^ self ensureMorphicAdapterDo: [ :adapter | adapter widthToDisplayInTree: aTree ]
]
