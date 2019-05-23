"
I am the adapter used to bridget a DropListPresenter and a DropListMorph
"
Class {
	#name : #MorphicDropListAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #factory }
MorphicDropListAdapter >> buildWidget [

	self model model whenChangedDo: [ 
		self changed: #getList.
		self changed: #getIndex  ].
	self model selection whenChangedDo: [ self changed: #getIndex ].

	^ SpecDropListMorph new
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
MorphicDropListAdapter >> displayedIcon [
	
	self model hasIcons ifFalse: [^ nil].
	^ self widget currentIcon
]

{ #category : #icons }
MorphicDropListAdapter >> getIconFor: aDropListItem [ 
	
	^ self model getIconFor: aDropListItem
]

{ #category : #'widget API' }
MorphicDropListAdapter >> getIndex [

	^ self model getIndex
]

{ #category : #'widget API' }
MorphicDropListAdapter >> getList [

	^ self model getList
]

{ #category : #accessing }
MorphicDropListAdapter >> selectIndex: anInteger [ 
	
	self widget listSelectionIndex: anInteger
]

{ #category : #accessing }
MorphicDropListAdapter >> selectedIndexes [
	
	| selectedIndex |
	selectedIndex := self widget getCurrentSelectionIndex.
	^ selectedIndex = 0 ifTrue: [ #() ] ifFalse: [ { selectedIndex } ]
]

{ #category : #'widget API' }
MorphicDropListAdapter >> setIndex: anIndex [
	^ self model selectedIndex: anIndex
]

{ #category : #'widget API' }
MorphicDropListAdapter >> wrapItem: anItem index: anIndex [
	| result |
	result := self model displayForItem: anItem.	
	^ result asString
]
