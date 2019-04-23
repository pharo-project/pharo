Class {
	#name : #MorphicMenuAdapter,
	#superclass : #MorphicBaseMenuAdapter,
	#category : #'Spec-MorphicAdapters-Base'
}

{ #category : #'instance creation' }
MorphicMenuAdapter class >> adaptAsPopup: aComposablePresenter [
	^ self new
		adaptAsPopup: aComposablePresenter;
		yourself
]

{ #category : #initialization }
MorphicMenuAdapter >> adaptAsPopup: aComposablePresenter [

	model := aComposablePresenter.
	aComposablePresenter addDependent: self.
	widget := self buildWidgetPopup.
]

{ #category : #private }
MorphicMenuAdapter >> basicMenuPopUp [

	^ MenuMorph new
		addIfNeededTitle: self title andIcon: self icon;
		yourself
]

{ #category : #private }
MorphicMenuAdapter >> buildWidget [
	
	widget := self basicMenuPopUp.
	self menuGroups
		do: [ :group | 
			| items |
			items := group buildWithSpec.
			items do: [ :item | widget addMorphBack: item ] ]
		separatedBy: [ widget addLine ].

	^ widget
]

{ #category : #private }
MorphicMenuAdapter >> buildWidgetPopup [
	
	^ self buildWidget
]
