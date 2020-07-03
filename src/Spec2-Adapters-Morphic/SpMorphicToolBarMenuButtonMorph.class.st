Class {
	#name : #SpMorphicToolBarMenuButtonMorph,
	#superclass : #Morph,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'instance creation' }
SpMorphicToolBarMenuButtonMorph class >> new [

	self error: 'Use newButton:menuButton:'
]

{ #category : #'instance creation' }
SpMorphicToolBarMenuButtonMorph class >> newButton: aButtonMorph menuButton: aMenuButton [

	^ self basicNew
		initializeButton: aButtonMorph menuButton: aMenuButton;
		yourself
]

{ #category : #accessing }
SpMorphicToolBarMenuButtonMorph >> button [ 

	^ self submorphs first
]

{ #category : #configuring }
SpMorphicToolBarMenuButtonMorph >> configureWith: aMode item: aButton [

	"transmit to button"
	self button
		configureWith: aMode 
		item: aButton
]

{ #category : #accessing }
SpMorphicToolBarMenuButtonMorph >> font: aFont [ 
	
	self submorphs do: [ :each |
		(each respondsTo: #font:) ifTrue: [ 
			each font: aFont ] ]
]

{ #category : #initialization }
SpMorphicToolBarMenuButtonMorph >> initialize [

	super initialize.
	self 
		changeTableLayout;
		listDirection: #leftToRight;
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		color: Color transparent;
		borderColor: Color transparent;
		clipSubmorphs: true;
		borderWidth: 0;
		extent: 0@0
]

{ #category : #initialization }
SpMorphicToolBarMenuButtonMorph >> initializeButton: aButtonMorph menuButton: aMenuButton [

	self initialize.
	self addMorphBack: (aButtonMorph 
		hResizing: #shrinkWrap;
		yourself).
	self addMorphBack: aMenuButton
]

{ #category : #accessing }
SpMorphicToolBarMenuButtonMorph >> menuButton [ 

	^ self submorphs second
]
