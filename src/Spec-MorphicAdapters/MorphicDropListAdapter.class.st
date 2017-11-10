"
I am the adapter used to bridget a DropListPresenter and a DropListMorph
"
Class {
	#name : #MorphicDropListAdapter,
	#superclass : #AbstractMorphicAdapter,
	#category : #Spec-MorphicAdapters
}

{ #category : #factory }
MorphicDropListAdapter >> buildWidget [

	^ SpecDropListMorph new
		wrapSelector:	 #wrap:withIndex:; 
	    	on: self list: #getList selected: #getIndex changeSelected: #setIndex:;
		hResizing: #spaceFill ;
		vResizing: #spaceFill ;
		dragEnabled: self dragEnabled;
		dropEnabled: self dropEnabled;	
		setBalloonText: self help;
		yourself
]

{ #category : #'widget API' }
MorphicDropListAdapter >> getIconFor: anItem [

	^ self model iconHolder cull: anItem model cull: anItem
]

{ #category : #'widget API' }
MorphicDropListAdapter >> getIndex [

	^ self model getIndex
]

{ #category : #'widget API' }
MorphicDropListAdapter >> getList [

	^ self model getList
]

{ #category : #'widget API' }
MorphicDropListAdapter >> setIndex: anIndex [
	^ self model setIndex: anIndex
]

{ #category : #'widget API' }
MorphicDropListAdapter >> wrapItem: anItem index: anIndex [
	| result |
	result := self model displayForItem: anItem.	
	^ result asString
]
