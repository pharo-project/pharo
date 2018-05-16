Class {
	#name : #GLMMorphic,
	#superclass : #Object,
	#category : #'Glamour-Morphic-Widgets'
}

{ #category : #'utilities - morph creation' }
GLMMorphic class >> alignmentMorph: aCollectionOfMorphs [
	"Answer a row-oriented AlignmentMorph holding the given collection"
	| morph |

	morph:= AlignmentMorph new
		listDirection: #leftToRight;
		borderWidth: 0;
		color: Color transparent;
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap;
		layoutInset: 1;
		wrapCentering: #center;
		cellPositioning: #center.
	aCollectionOfMorphs do: [ :each | morph addMorphBack: each].
	^morph
]

{ #category : #'utilities - morph creation' }
GLMMorphic class >> containerMorph [
	^ GLMPanelMorph new
		layoutPolicy: ProportionalLayout new;
		hResizing: #spaceFill;
		vResizing: #spaceFill;
		borderWidth: 0;
		yourself
]

{ #category : #'utilities - morph creation' }
GLMMorphic class >> emptyMorph [
	^ self containerMorph
		fillStyle: (SolidFillStyle color: Color transparent );
		yourself
]

{ #category : #'button styles' }
GLMMorphic class >> grayRoundedButtonStyle: button morph: aMorph pressed: aBoolean [
	button useRoundedCorners.
	(self isAlignmentMorph: aMorph)
		ifTrue: [
			aBoolean 
				ifTrue: [	aMorph lastSubmorph color: Color gray.
						button fillStyle: (button theme buttonSelectedFillStyleFor: button).
						button borderStyle: (button theme buttonSelectedBorderStyleFor: button) ]
				ifFalse:[	aMorph lastSubmorph color: Color white.
						button fillStyle: (button theme buttonNormalFillStyleFor: button).
						button borderStyle: (button theme buttonNormalBorderStyleFor: button) ] ]
		ifFalse: [			
			aBoolean 
				ifTrue: [	aMorph color: Color gray.
						button color: Color veryLightGray ]
				ifFalse:[	aMorph color: Color white.
						button color: Color lightGray ] ]

]

{ #category : #testing }
GLMMorphic class >> isAlignmentMorph: anObject [

	^anObject isMemberOf: AlignmentMorph
]

{ #category : #'button styles' }
GLMMorphic class >> isGrayRoundedButton: aSymbol [

	^aSymbol = #grayRoundedButton
		or:[ aSymbol = #default ]
]

{ #category : #testing }
GLMMorphic class >> isImageMorph: anObject [

	^anObject isMemberOf: ImageMorph
]

{ #category : #'button styles' }
GLMMorphic class >> isWhiteRectangledButton: aSymbol [
	^aSymbol = #whiteRectangledButton
]

{ #category : #'utilities - morph creation' }
GLMMorphic class >> morphElement: anObject [

	anObject isString
	ifTrue:[ ^StringMorph 
					contents: anObject 
					font: (LogicalFont 
						familyName: UITheme current buttonFont familyName
						pointSize: UITheme current buttonFont pointSize - 2) ].
	
	anObject isCollection 
	ifTrue: [ |row|
				row:= OrderedCollection new.
				anObject do: [ :e| row add: (self morphElement: e) ].
				^self alignmentMorph: row ].
				
	anObject isForm
	ifTrue:[ ^ImageMorph new form: anObject ].
	
	^self containerMorph
]

{ #category : #'button styles' }
GLMMorphic class >> styleButton: button morph: m pressed: aBoolean style: aSymbol [
	(self isGrayRoundedButton: aSymbol)
		ifTrue:[ self grayRoundedButtonStyle: button morph: m pressed: aBoolean ].
	
	(self isWhiteRectangledButton: aSymbol)
		ifTrue:[ self whiteRectangledButtonStyle: button morph: m pressed: aBoolean ]
]

{ #category : #'utilities - morph creation' }
GLMMorphic class >> togglingButtonLabelled: anObject pressed: aBoolean style: aSymbol [
	| button oldLabel m |
	button := SimpleButtonMorph new.
	(oldLabel := button findA: StringMorph) ifNotNil: [ oldLabel delete ].
	m := self morphElement: anObject.
	button
		addMorph: m;
		borderColor: Color transparent.
	(self isAlignmentMorph: m)
		ifTrue:[ 
			button extent: (m width + 6 ) @ (m height + 6 ).
			m position: 3 @ 3 ]
		ifFalse:[ 
			(self isImageMorph: m)
				ifTrue: [ button extent: (m width + 6) @ (m height + 6) ]
				ifFalse:[ button extent: (m width + 6) @ (m height + 6) ].
			m position: 3 @ 3.	"button center - (m extent // 2)" ].
	self styleButton: button morph: m pressed: aBoolean style: aSymbol.
	m lock.
	^ button
]

{ #category : #'button styles' }
GLMMorphic class >> whiteRectangledButtonStyle: button morph: aMorph pressed: aBoolean [

	button useSquareCorners.
	(self isAlignmentMorph: aMorph)
	ifTrue:[ 
		aBoolean 
		ifTrue: [ aMorph lastSubmorph color: Color white.
				      button borderColor: Color gray.
					button color: Color gray ]
		ifFalse:[ aMorph lastSubmorph color: Color black.
			            button borderColor: Color gray.
					button color: Color white ].
		button extent: aMorph width @ (aMorph height - 2).
		aMorph position: 0 @ 0 ]
	ifFalse:[			
		aBoolean 
		ifTrue: [ aMorph color: Color white.
			         button borderColor: Color gray.
				   button color: Color gray ]
		ifFalse:[ aMorph color: Color black.
			            button borderColor: Color gray.
					button color: Color white ].
		button extent: (aMorph width + 6) @ (aMorph height + 2).
		aMorph position: 3 @ 1 ]
]
