Class {
	#name : #SpMorphicToolbarMenuButtonMorph,
	#superclass : #Morph,
	#category : #'Spec2-Adapters-Morphic-Base'
}

{ #category : #'instance creation' }
SpMorphicToolbarMenuButtonMorph class >> new [

	self error: 'Use newButton:menuButton:'
]

{ #category : #'instance creation' }
SpMorphicToolbarMenuButtonMorph class >> newButton: aButtonMorph menuButton: aMenuButton [

	^ self basicNew
		initializeButton: aButtonMorph menuButton: aMenuButton;
		yourself
]

{ #category : #accessing }
SpMorphicToolbarMenuButtonMorph >> button [ 

	^ self submorphs first
]

{ #category : #configuring }
SpMorphicToolbarMenuButtonMorph >> configureWith: aMode item: aButton [

	"transmit to button"
	self button
		configureWith: aMode 
		item: aButton
]

{ #category : #accessing }
SpMorphicToolbarMenuButtonMorph >> font: aFont [ 
	
	self submorphs do: [ :each |
		(each respondsTo: #font:) ifTrue: [ 
			each font: aFont ] ]
]

{ #category : #initialization }
SpMorphicToolbarMenuButtonMorph >> initialize [

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
SpMorphicToolbarMenuButtonMorph >> initializeButton: aButtonMorph menuButton: aMenuButton [

	self initialize.
	self addMorphBack: (aButtonMorph 
		hResizing: #shrinkWrap;
		yourself).
	self addMorphBack: aMenuButton
]

{ #category : #accessing }
SpMorphicToolbarMenuButtonMorph >> menuButton [ 

	^ self submorphs second
]
