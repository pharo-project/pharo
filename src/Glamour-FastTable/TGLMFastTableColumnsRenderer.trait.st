"
I am a Trait that provides methods for creating rows in a Glamour presentation with columns. I exist mainly ro provides code reuse. 

Description 
--------------------

My main entry point is #cellColumn:row:. This is the method called by FTTableMorph on a data source for obtaining the rows to display.  I can configure rows to also be editable.


Internal Representation and Key Implementation Points.
--------------------

- #rowMorphElementsForFirstColumn:item:in:    I am a  hook method for configuring the first column (tables and tree need a different first column)
- #dataFromPresentationItem:                                     I am a hook methods for getting the actual data from an item of the FTTableMorph
"
Trait {
	#name : #TGLMFastTableColumnsRenderer,
	#category : #'Glamour-FastTable-Model'
}

{ #category : #rendering }
TGLMFastTableColumnsRenderer >> cellColumn: aColumn row: aRowIndex [
	| item cell |
	item := self elementAt: aRowIndex.
	cell := FTCellMorph new
		cellInset: 5;
		addMorph: (self 
			rowMorphForColumn: aColumn 
			item: item 
			withIndex: aRowIndex);
		yourself.
	^ cell
]

{ #category : #accessing }
TGLMFastTableColumnsRenderer >> dataFromPresentationItem: anItem [
	self explicitRequirement
]

{ #category : #rendering }
TGLMFastTableColumnsRenderer >> editRowMorphForColumn: glamourColumn node: aNode withIndex: aRowIndex [ 
	| rowMorph |

	rowMorph := self viewRowMorphForColumn: glamourColumn node: aNode withIndex: aRowIndex.
	rowMorph hResizing: #spaceFill.
	rowMorph
		on: #mouseDown
		send: #value:
		to: [ :event | 
			event shiftPressed ifTrue: [
				GLMEditorPopupBrick new
					in: [ :aPopup | aPopup themer: aPopup themer popupEditorBorderThemer themer ];
					closer: (GLMPopupBrickCloser new
						predicate: GLMPopupBrickOutisideClickPredicate new;
						predicate: GLMPopupBrickEscPredicate new;
						yourself);
					textModel: (self valueFor: aNode withIndex: aRowIndex in: glamourColumn);
					when: #textAccepted do: [ :anEvent | glamourColumn modifiedBlock
						cull: anEvent morph text
						cull: aNode
						cull: self glamourPresentation ];
					openOn: rowMorph ]
				ifFalse: [ event wasHandled: false ] ].
	
	^ rowMorph

]

{ #category : #rendering }
TGLMFastTableColumnsRenderer >> rowMorphElementsForColumn: glamourColumn node: aNode withIndex: aRowIndex [ 
	| rowElements contentMorph tagMorphs |
		
	contentMorph := glamourColumn modifiedBlock
		ifNotNil: [ self editRowMorphForColumn: glamourColumn node: aNode withIndex: aRowIndex ]
		ifNil: [ self viewRowMorphForColumn: glamourColumn node: aNode withIndex: aRowIndex ].
	
	rowElements := OrderedCollection new
		add: contentMorph;
		yourself.
	
	tagMorphs := self tagMorphsFrom: glamourColumn for: aNode.
	tagMorphs ifNotEmpty: [
		rowElements addAll: tagMorphs ].
	
	^ rowElements
]

{ #category : #rendering }
TGLMFastTableColumnsRenderer >> rowMorphElementsForFirstColumn: aColumn item: anItem in: rowElements [
	self explicitRequirement  
]

{ #category : #rendering }
TGLMFastTableColumnsRenderer >> rowMorphForColumn: aColumn item: anItem withIndex: aRowIndex [
	| rowElements |
	rowElements := OrderedCollection new.
	
	(self table columns first == aColumn) ifTrue: [
		self rowMorphElementsForFirstColumn: aColumn item: anItem in: rowElements ].
	
	rowElements
		addAll: (self 
			rowMorphElementsForColumn: aColumn glamourColumn 
			node: (self dataFromPresentationItem: anItem)
			withIndex: aRowIndex ).
		
	^ Smalltalk ui theme newRowIn: self currentWorld for: rowElements
]

{ #category : #'accessing - cache' }
TGLMFastTableColumnsRenderer >> valueFor: anElement withIndex: anInteger in: aGlamourColumn [
	
	^ self 
		cacheAt: aGlamourColumn title 
		for: anElement 
		ifAbsentPut: [ 
			self glamourPresentation 
				column: aGlamourColumn 
				valueFor: (self glamourPresentation isWithElementIndex
					ifTrue: [ 
						anElement asGlamorousMultiValue,
						anInteger asGlamorousMultiValue ]
			ifFalse: [ anElement ]) ]


	
]

{ #category : #rendering }
TGLMFastTableColumnsRenderer >> viewRowMorphForColumn: aGlamourColumn node: aNode withIndex: anInteger [
	^ (self valueFor: aNode withIndex: anInteger in: aGlamourColumn) asMorph asReadOnlyMorph
]
