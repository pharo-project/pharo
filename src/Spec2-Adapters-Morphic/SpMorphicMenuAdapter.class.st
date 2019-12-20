Class {
	#name : #SpMorphicMenuAdapter,
	#superclass : #SpMorphicBaseMenuAdapter,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'instance creation' }
SpMorphicMenuAdapter class >> adaptAsPopup: aPresenter [
	^ self new
		adaptAsPopup: aPresenter;
		yourself
]

{ #category : #initialization }
SpMorphicMenuAdapter >> adaptAsPopup: aPresenter [

	model := aPresenter.
	aPresenter addDependent: self.
	widget := self buildWidgetPopup.
]

{ #category : #private }
SpMorphicMenuAdapter >> basicMenuPopUp [

	^ MenuMorph new
		addIfNeededTitle: self title andIcon: self icon;
		yourself
]

{ #category : #private }
SpMorphicMenuAdapter >> buildWidget [
	
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
SpMorphicMenuAdapter >> buildWidgetPopup [
	
	^ self buildWidget
]
