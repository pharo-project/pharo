"
I am a MorphTreeMorph, but I only render the rows lazily as they appear on screen. 

Caveat: There is no rendering in the background. This means that if a row takes too long to render, you can experience a shorter or longer freeze when scrolling.
"
Class {
	#name : #LazyMorphTreeMorph,
	#superclass : #MorphTreeMorph,
	#instVars : [
		'lazyIncrement'
	],
	#category : 'Morphic-Widgets-Tree'
}

{ #category : #'initialize - release' }
LazyMorphTreeMorph >> adjustSubmorphPositionsFrom: start to: stop [
	"Fixed to not require setting item widths to 9999."
	
	| subs p |
	subs := self allNodeMorphs.
	p := (subs at: start) position.
	start to: subs size do: [:idx | | each h |
		each := subs at: idx.
		h := each height.
		each bounds: (p extent: each width@h).
		p := p + (0@h)].

]

{ #category : #'initialize - release' }
LazyMorphTreeMorph >> buildRowMorphsFrom: aNodeMorph [
	self buildRowMorphsFrom: aNodeMorph increment: self lazyIncrement.

]

{ #category : #'initialize - release' }
LazyMorphTreeMorph >> buildRowMorphsFrom: aNodeMorph increment: anIncrement [
	
	Cursor wait 
		showWhile: [ | subs idx max |
			subs := self allNodeMorphs.
			idx :=  aNodeMorph index.
			max := (idx + anIncrement) min: subs size.
			self buildRowMorphsFromIndex: idx to: max].
]

{ #category : #'initialize - release' }
LazyMorphTreeMorph >> buildRowMorphsFromIndex: startIndex to: stopIndex [
	| rowMorphsWidths subs |
	subs := self allNodeMorphs.
	rowMorphsWidths := self rowMorphsWidths.
	startIndex to: stopIndex
		do: [:p | (subs at: p) buildRowMorph; updateColumnMorphsWidthWith: rowMorphsWidths].
	self setScrollDeltas.
	self adjustSubmorphPositionsFrom: startIndex to: stopIndex.
	self setScrollDeltas.
	startIndex to: stopIndex do: [:pos | (subs at: pos) drawable: true].

]

{ #category : #'lazy tree' }
LazyMorphTreeMorph >> indentingItemClass [
	
	^ LazyMorphTreeNodeMorph
]

{ #category : #'initialize - release' }
LazyMorphTreeMorph >> lazyIncrement [
	^ lazyIncrement ifNil: [ lazyIncrement := 20 ] 
]

{ #category : #'initialize - release' }
LazyMorphTreeMorph >> lazyIncrement: anInteger [
	lazyIncrement := anInteger 
]
