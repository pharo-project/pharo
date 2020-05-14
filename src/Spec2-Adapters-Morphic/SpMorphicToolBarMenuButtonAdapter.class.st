"
I'm the adapter used to bridge a ToolBarMenuButton and its morphic representation.
"
Class {
	#name : #SpMorphicToolBarMenuButtonAdapter,
	#superclass : #SpMorphicToolBarButtonAdapter,
	#instVars : [
		'subMenuWidget'
	],
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #protocol }
SpMorphicToolBarMenuButtonAdapter >> askBeforeChanging [

	^ false
]

{ #category : #accessing }
SpMorphicToolBarMenuButtonAdapter >> badge [
	
	^ self presenter badge
]

{ #category : #factory }
SpMorphicToolBarMenuButtonAdapter >> buildWidget [
	
	^ SpMorphicToolBarMenuButtonMorph  
		newButton: super buildWidget
		menuButton: self newMenuButton
]

{ #category : #updating }
SpMorphicToolBarMenuButtonAdapter >> button [

	^ widget submorphs first
]

{ #category : #control }
SpMorphicToolBarMenuButtonAdapter >> deleteIfPopUp: anEvent [ 
	
]

{ #category : #'drag and drop' }
SpMorphicToolBarMenuButtonAdapter >> dragEnabled [

	^ false
]

{ #category : #'drag and drop' }
SpMorphicToolBarMenuButtonAdapter >> dropEnabled [

	^ false
]

{ #category : #accessing }
SpMorphicToolBarMenuButtonAdapter >> icon [

	^ self presenter icon
]

{ #category : #testing }
SpMorphicToolBarMenuButtonAdapter >> isSelected: aBooolean [
]

{ #category : #accessing }
SpMorphicToolBarMenuButtonAdapter >> label [
	
	^ self presenter label
]

{ #category : #accessing }
SpMorphicToolBarMenuButtonAdapter >> menuIcon [

	^ self application iconNamed: #arrowDown
]

{ #category : #factory }
SpMorphicToolBarMenuButtonAdapter >> newMenuButton [

	^ IconicButton new
		target: self;
		actionSelector: #showMenu;
		arguments: {};
		labelGraphic: self menuIcon;
		color: Color transparent;
		borderWidth: 0
]

{ #category : #accessing }
SpMorphicToolBarMenuButtonAdapter >> showMenu [

	self presenter menu ifNotNil: [ :aValuable |
		aValuable value ifNotNil: [ :aMenu |
			aMenu owner: self presenter.
			self showSubMenu: aMenu ] ].
	self widget ifNotNil: [ :m | m takeKeyboardFocus ] 
]

{ #category : #'widget API' }
SpMorphicToolBarMenuButtonAdapter >> showSubMenu: aMenu [ 

	aMenu menuGroups ifNil: [ ^ self ].
	 
	subMenuWidget ifNotNil: [ 
		subMenuWidget delete.
		subMenuWidget := nil ].
	subMenuWidget := (SpMorphicMenuAdapter adaptAsPopup: aMenu) widget.
	subMenuWidget
		popUpAdjacentTo: { 
			self widget bounds bottomLeft.
			self widget bounds bottomRight }
		forHand: self currentWorld activeHand
		from: self widget.
	subMenuWidget popUpOwner: self
]
