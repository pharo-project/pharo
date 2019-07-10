"
I am a column controller. My responsability is to provide a convenient API to Hiedra users, either in Spec (TreePresenter) or Morph (FastTable). Basically, a table widget that shows a Hiedra visualization in a column will obtain the ""cell morphs"" that correspond to each row of the table. 

I implement a pagination mechanism to split the rendering of a HiRuler in pages of a fixed size on demand.

My most important collaborator is a Hiedra renderer (subclass of HiAbstractRenderer) that the user can personalize with appearance settings, and that my instances use to render the pages forms on demand.
"
Class {
	#name : #HiColumnController,
	#superclass : #Object,
	#instVars : [
		'renderer',
		'formByPageIndex',
		'pageSize',
		'cellMorphByRowIndex'
	],
	#category : #'Hiedra-UI'
}

{ #category : #'system startup' }
HiColumnController class >> initialize [
	SessionManager default registerGuiClassNamed: self name
]

{ #category : #'system startup' }
HiColumnController class >> startUp: isImageStarting [
	"Reset all instances on startup since the cached pages (athens forms) don't work in a new session."

	isImageStarting
		ifTrue: [ self allInstancesDo: #reset ]
]

{ #category : #private }
HiColumnController >> cellBoundsForRow: rowIndex inPage: pageIndex [
	
	| rowIndexRelativeToRuler rowIndexRelativeToPage rowIndexOfFirstNodeInPage |
	rowIndexRelativeToRuler := (self ruler nodeAtRow: rowIndex) rulerPoint y.
	rowIndexOfFirstNodeInPage := self rowIndexOfFirstItemAtPage: pageIndex.
	rowIndexRelativeToPage := rowIndexRelativeToRuler - rowIndexOfFirstNodeInPage.

	^ 0 @ (rowIndexRelativeToPage * renderer rowHeight)
		corner: self rulerWidth @ ((rowIndexRelativeToPage + 1) * renderer rowHeight)
]

{ #category : #API }
HiColumnController >> cellMorphAtRow: rowIndex [
	"Answer a Morph that corresponds to a row index. The height of such Morph is given by renderer's rowHeight and the width is given by the ruler's numberOfColumns and the renderer's cellWidth.
	This is an important method of this class, that the table widget will use to fill each Hiedra column's cell."

	^ cellMorphByRowIndex at: rowIndex ifAbsent: [ self newCellMorphForRow: rowIndex ]
]

{ #category : #API }
HiColumnController >> cellMorphAtValue: aValue [
	"Answer a Morph that corresponds to aValue. See #cellMorphAtRowIndex: for more information."

	^ self cellMorphAtRow: (self rowIndexAtValue: aValue)
]

{ #category : #private }
HiColumnController >> formAtPage: pageIndex [

	^ formByPageIndex
		at: pageIndex
		ifAbsentPut: [
			renderer
				rowsInterval: (self rowIntervalAtPage: pageIndex);
				newForm ]
]

{ #category : #initialization }
HiColumnController >> initialize [

	super initialize.
	renderer := HiSimpleRenderer new.
	pageSize := 30.
	self reset.
]

{ #category : #private }
HiColumnController >> newCellMorphForRow: rowIndex [
	"Answer a Morph that corresponds to aValue in the rendered ruler."

	| pageIndex aForm cellBoundsInPage |
	pageIndex := self pageIndexAtRow: rowIndex.
	cellBoundsInPage := self
		cellBoundsForRow: rowIndex
		inPage: pageIndex.

	aForm := Form
		extent: cellBoundsInPage extent
		depth: 32.
	aForm getCanvas
		translucentImage: (self formAtPage: pageIndex)
		at: 0 @ 0
		sourceRect: cellBoundsInPage truncated.
	^ aForm asMorph
]

{ #category : #private }
HiColumnController >> pageIndexAtRow: rowIndex [

	^ (rowIndex - 1) // self pageSize
]

{ #category : #API }
HiColumnController >> pageSize [
	^ pageSize
]

{ #category : #API }
HiColumnController >> pageSize: anInteger [
	"Set the number of rows that will be rendered together (when I'm asked for a missing cell Morph)."

	pageSize := anInteger
]

{ #category : #API }
HiColumnController >> renderer [
	^ renderer
]

{ #category : #API }
HiColumnController >> renderer: aHiRulerRenderer [
	renderer := aHiRulerRenderer
]

{ #category : #API }
HiColumnController >> reset [
	"Reset the internal state with the purpose to regenerate the visualization the next time #cellMorphFor: is executed."

	cellMorphByRowIndex := Dictionary new.
	formByPageIndex := Dictionary new.
]

{ #category : #private }
HiColumnController >> rowIndexAtValue: aValue [
	^ self ruler valueIndices
		at: aValue
		ifAbsent: [ self error: 'Value not found. Should the table refresh the ruler?' ]
]

{ #category : #private }
HiColumnController >> rowIndexOfFirstItemAtPage: pageIndex [
	^ pageIndex * self pageSize + 1
]

{ #category : #private }
HiColumnController >> rowIntervalAtPage: pageIndex [
	"Answer the interval of row indices that correspond to the specified page index."

	^ 	(self rowIndexOfFirstItemAtPage: pageIndex) to: 
		(((pageIndex + 1) * self pageSize) min: self ruler numberOfRows)
]

{ #category : #API }
HiColumnController >> ruler [
	^ renderer ruler
]

{ #category : #API }
HiColumnController >> ruler: aHiRuler [
	renderer ruler: aHiRuler
]

{ #category : #API }
HiColumnController >> rulerWidth [

	^ renderer formWidth
]
