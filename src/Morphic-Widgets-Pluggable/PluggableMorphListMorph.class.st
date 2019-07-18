"
A type of PluggableListMorph that supports morphs for items. Useful for lists with icons etc.
"
Class {
	#name : #PluggableMorphListMorph,
	#superclass : #PluggableListMorph,
	#category : #'Morphic-Widgets-Pluggable-Lists'
}

{ #category : #geometry }
PluggableMorphListMorph >> extent: newExtent [
	"Change listMorph's bounds to the new width. It is either the size
	of the widest list item, or the size of self, whatever is bigger"

	super extent: newExtent.
	self listMorph width: (self innerBounds width max: listMorph hUnadjustedScrollRange). 

]

{ #category : #'model access' }
PluggableMorphListMorph >> getList [

	"Answer the list to be displayed.  Caches the returned list in the 'list' ivar"

	getListSelector ifNil: [ ^ #() ].
	list := model perform: getListSelector.
	list ifNil: [ ^ #() ].
	^ list
]

{ #category : #scrolling }
PluggableMorphListMorph >> hExtraScrollRange [
	"Return the amount of extra blank space to include to the right of the scroll content."

	^12
]

{ #category : #initialization }
PluggableMorphListMorph >> listMorphClass [
	"Answer the class to use for the list morph."

	^LazyMorphListMorph
]
