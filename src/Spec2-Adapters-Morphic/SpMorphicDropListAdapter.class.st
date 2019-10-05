"
I am the adapter used to bridget a DropListPresenter and a DropListMorph
"
Class {
	#name : #SpMorphicDropListAdapter,
	#superclass : #SpAbstractMorphicAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #factory }
SpMorphicDropListAdapter >> buildWidget [

	self model model whenChangedDo: [ 
		self changed: #getList.
		self changed: #getIndex  ].
	self model selection whenChangedDo: [ self changed: #getIndex ].

	^ SpDropListMorph new
		wrapSelector: #wrap:withIndex:;
		on: self list: #getList selected: #getIndex changeSelected: #setIndex:;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;
		enabled: self enabled;
		setBalloonText: self help;
		yourself
]

{ #category : #accessing }
SpMorphicDropListAdapter >> displayedIcon [
	
	self model hasIcons ifFalse: [^ nil].
	^ self widget currentIcon
]

{ #category : #icons }
SpMorphicDropListAdapter >> getIconFor: aDropListItem [ 
	
	^ self model getIconFor: aDropListItem
]

{ #category : #'widget API' }
SpMorphicDropListAdapter >> getIndex [

	^ self model getIndex
]

{ #category : #'widget API' }
SpMorphicDropListAdapter >> getList [

	^ self model getList
]

{ #category : #accessing }
SpMorphicDropListAdapter >> selectIndex: anInteger [ 
	
	self widget listSelectionIndex: anInteger
]

{ #category : #accessing }
SpMorphicDropListAdapter >> selectedIndexes [
	
	| selectedIndex |
	selectedIndex := self widget getCurrentSelectionIndex.
	^ selectedIndex = 0 ifTrue: [ #() ] ifFalse: [ { selectedIndex } ]
]

{ #category : #'widget API' }
SpMorphicDropListAdapter >> setIndex: anIndex [
	^ self model selectIndex: anIndex
]

{ #category : #'widget API' }
SpMorphicDropListAdapter >> wrapItem: anItem index: anIndex [
	| result |
	result := self model displayForItem: anItem.	
	^ result asString
]
