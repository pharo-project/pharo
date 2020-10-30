"
A morph to show a paginator.
A paginator is just another way to offer ""scrolling"" of contents: instead having a bar, we have selectable bullers (pages) that represent part of the contents.

Users can add pages by calling `SpPaginatorMorph>>#addPage:` or `SpPaginatorMorph>>#addPages:`. Amount of pages to be part of the selected area (shown) are handled through `SpPaginatorMorph>>#numberOfPagesShown:`.
"
Class {
	#name : #SpPaginatorMorph,
	#superclass : #Morph,
	#traits : 'TObservable',
	#instVars : [
		'#pagesMorph',
		'#selectionMorph',
		'#selectedPage => ObservableSlot',
		'#numberOfPagesShown => ObservableSlot'
	],
	#category : #'Spec2-Adapters-Morphic-Paginator'
}

{ #category : #accessing }
SpPaginatorMorph class >> defaultHeight [

	^ 18
]

{ #category : #accessing }
SpPaginatorMorph class >> defaultNumberOfPagesShown [

	^ 2
]

{ #category : #accessing }
SpPaginatorMorph >> addPage: anObject [

	pagesMorph addMorphBack: (SpPaginatorPageMorph new
		vResizing: #spaceFill;
		model: anObject;
		whenSelectedDo: [ :aMorph | self doSelectPageFromMorph: aMorph ];
		yourself).
	self width: pagesMorph submorphs size * self baseWidth
]

{ #category : #accessing }
SpPaginatorMorph >> addPages: aCollection [

	aCollection do: [ :each | self addPage: each ]
]

{ #category : #private }
SpPaginatorMorph >> adjustSelectionToClosestPage [
	| closestPage |

	closestPage := ((selectionMorph left - self left) / self baseWidth) rounded + 1.	
	selectionMorph left: self left + ((closestPage - 1) * self baseWidth).
	
	closestPage = self selectedPage 
		ifFalse: [ self basicSelectPage: closestPage ]
]

{ #category : #private }
SpPaginatorMorph >> adjustShownPages [
	| newShownPages |

	newShownPages := (selectionMorph width / self baseWidth) asInteger.
	self numberOfPagesShown = newShownPages 
		ifFalse: [ self numberOfPagesShown: newShownPages ]
]

{ #category : #private }
SpPaginatorMorph >> adjustValues [

	self adjustSelectionToClosestPage.
	self adjustShownPages
]

{ #category : #private }
SpPaginatorMorph >> baseWidth [

	^ SpPaginatorPageMorph baseWidth
]

{ #category : #private }
SpPaginatorMorph >> basicSelectPage: aNumber [
	
	selectedPage := aNumber
]

{ #category : #geometry }
SpPaginatorMorph >> bounds: aRectangle [

	super bounds: aRectangle.
	selectionMorph height: aRectangle height
]

{ #category : #initialization }
SpPaginatorMorph >> defaultColor [

	^ Color transparent
]

{ #category : #private }
SpPaginatorMorph >> doSelectPageFromMorph: aMorph [

	self selectPage: (pagesMorph submorphs indexOf: aMorph)
]

{ #category : #initialization }
SpPaginatorMorph >> extent: aPoint [

	super extent: aPoint.
	pagesMorph height: aPoint y.
	selectionMorph height: aPoint y
]

{ #category : #initialization }
SpPaginatorMorph >> initialize [

	self class initializeSlots: self.
	super initialize.
	self 
		vResizing: #shrinkWrap;
		hResizing: #shrinkWrap.

	self addMorphBack: (selectionMorph := self newSelectionMorph).
	self addMorphBack: (pagesMorph := self newPagesMorph).
	
	self numberOfPagesShown: self class defaultNumberOfPagesShown.
	"We select the first page first"
	self basicSelectPage: 1.
]

{ #category : #private }
SpPaginatorMorph >> moveNext [
	self flag: #REMOVE. 
	selectionMorph left: (((selectionMorph left + selectionMorph width) + self baseWidth) > self right 
		ifTrue: [ 0 ]
		ifFalse: [ selectionMorph left + self baseWidth ])
]

{ #category : #private }
SpPaginatorMorph >> moveSelectionToPage: aNumber [

	selectionMorph left: self left + ((aNumber - 1) * self baseWidth)
]

{ #category : #initialization }
SpPaginatorMorph >> newPagesMorph [

	^ Morph new 
		color: Color transparent;
		changeTableLayout; 
		listDirection: #leftToRight; 
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		yourself
]

{ #category : #initialization }
SpPaginatorMorph >> newSelectionMorph [
	
	^ SpPaginatorSelectionMorph new
		whenDragFinishedDo: [ self adjustValues ];
		yourself
]

{ #category : #accessing }
SpPaginatorMorph >> numberOfPagesShown [

	^ numberOfPagesShown
]

{ #category : #accessing }
SpPaginatorMorph >> numberOfPagesShown: aNumber [

	numberOfPagesShown := aNumber.
	selectionMorph updateSizePages: numberOfPagesShown pageWidth: self baseWidth
]

{ #category : #accessing }
SpPaginatorMorph >> removeAllPages [

	pagesMorph removeAllMorphs
]

{ #category : #accessing }
SpPaginatorMorph >> removePage: anObject [

	pagesMorph removeMorph: (pagesMorph submorphs 
		detect: [ :each | each model = anObject ]) 
]

{ #category : #accessing }
SpPaginatorMorph >> selectLastPage [

	self selectPage: (pagesMorph submorphs size)
]

{ #category : #accessing }
SpPaginatorMorph >> selectPage: aNumber [

	selectedPage = aNumber ifTrue: [ ^ self ].
	(aNumber + self numberOfPagesShown - 1 > pagesMorph submorphs size) 
		ifTrue: [ ^ self selectPage: aNumber - 1 ].

	self moveSelectionToPage: aNumber.
	self basicSelectPage: aNumber
]

{ #category : #accessing }
SpPaginatorMorph >> selectedPage [

	^ selectedPage
]

{ #category : #accessing }
SpPaginatorMorph >> selectedPages [
	
	^ (self selectedPage to: self selectedPage + self numberOfPagesShown - 1) asArray
]

{ #category : #accessing }
SpPaginatorMorph >> selectionMorph [

	^ selectionMorph
]

{ #category : #events }
SpPaginatorMorph >> whenNumberOfPagesShownChangedDo: aBlock [ 

	self property: #numberOfPagesShown whenChangedDo: aBlock
]

{ #category : #events }
SpPaginatorMorph >> whenSelectedPageChangedDo: aBlock [ 

	self property: #selectedPage whenChangedDo: aBlock
]
