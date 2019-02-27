"
I am the adapter used to bridge an IconListPresenter and a PluggableIconListMorph
"
Class {
	#name : #MorphicIconListAdapter,
	#superclass : #MorphicListAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #factory }
MorphicIconListAdapter >> buildWidget [
	^ super buildWidget
		getIconSelector: #getIconFor:;
		yourself
]

{ #category : #'widget API' }
MorphicIconListAdapter >> getIconFor: anItem [

	^ self model getIconFor: anItem
]
