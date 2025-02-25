"
A join section usable for unified diff 

"
Class {
	#name : 'UnifiedDiffJoinSection',
	#superclass : 'JoinSection',
	#instVars : [
		'copyJoin'
	],
	#category : 'Tool-Diff-Morphs',
	#package : 'Tool-Diff',
	#tag : 'Morphs'
}

{ #category : 'copying' }
UnifiedDiffJoinSection >> copy: anObject [

	copyJoin := anObject
]

{ #category : 'copying' }
UnifiedDiffJoinSection >> copyJoin [

	^ copyJoin
]

{ #category : 'copying' }
UnifiedDiffJoinSection >> copyLineRange: anInterval [

	"Set the src lneRange."

	self copyJoin lineRange: anInterval
]

{ #category : 'copying' }
UnifiedDiffJoinSection >> copyRange: anInterval [

	"Set the  copy range"

	self copyJoin range: anInterval.
	self updateShape
]

{ #category : 'actions' }
UnifiedDiffJoinSection >> createHighlights [

	"Create and store the src and dst highlights."

	| s d c |
	s := OrderedCollection new.
	d := OrderedCollection new.
	c := OrderedCollection new.
	s add: (self newHighlight
			 color: self src color;
			 borderWidth: self borderWidth;
			 bounds:
				 (0 @ self src range first corner: 0 @ (self src range last + 1));
			 borderSides: #( top left bottom )).
	d add: (self newHighlight
			 color: self dst color;
			 borderWidth: self borderWidth;
			 bounds:
				 (0 @ self dst range first corner: 0 @ (self dst range last + 1));
			 borderSides: #( top right bottom )).
	c add: (self newHighlight
			 color: self dst color;
			 borderWidth: self borderWidth;
			 bounds:
				 (0 @ self copyJoin range first corner:
						  0 @ (self copyJoin range last + 1));
			 borderSides: #( top right bottom )).
	self src highlights: s.
	self dst highlights: d.
	self copyJoin highlights: c
]

{ #category : 'copying' }
UnifiedDiffJoinSection >> createHighlightsFrom: srcPara to: dstPara withCopy: copyPara flagRemove: aNumber [

	"Create and store the src and dst highlights.
	Use the given paragraphs to determine inline
	diffs."

	| d si di dri ci srcText copyText diffs i sb eb line |
	self createHighlights.
	self src lineRange notEmpty
		ifTrue: [ 
			line := srcPara lines at: self src lineRange first.
			si := line first.
			line := srcPara lines at: self src lineRange last.
			srcText := srcPara string copyFrom: si to: line last ]
		ifFalse: [ srcText := '' ].
	self dst lineRange notEmpty ifTrue: [ 
		line := dstPara lines at: self dst lineRange first.
		di := line first.
		line := dstPara lines at: self dst lineRange first + aNumber.
		dri := line first ].
	self copyJoin lineRange notEmpty
		ifTrue: [ 
			line := copyPara lines at: self copyJoin lineRange first.
			ci := line first.
			line := copyPara lines at: self copyJoin lineRange last.
			copyText := copyPara string copyFrom: ci to: line last ]
		ifFalse: [ copyText := '' ].
	self src text: srcText.
	self copyJoin text: copyText.
	self type = #modification ifFalse: [ ^ self ].
	d := self dst highlights.
	diffs := (InlineTextDiffBuilder from: srcText to: copyText)
		         buildPatchSequence groupByRuns: [ :e | e key ].

	diffs do: [ :c | 
		c first key = #match ifTrue: [ 
			c do: [ :a | 
				si := si + a value size.
				di := di + a value size.
				ci := ci + a value size.
				dri := dri + a value size ] ].
		c first key = #insert ifTrue: [ 
			i := di.
			c do: [ :a | 
				ci := ci + a value size.
				di := di + a value size ].
			sb := dstPara characterBlockForIndex: i.
			eb := dstPara characterBlockForIndex: di - 1.
			self
				addHighlightsFrom: sb
				to: eb
				to: d
				color: self additionHighlightColor ].
		c first key = #remove ifTrue: [ 
			i := dri.
			c do: [ :a | 
				si := si + a value size.
				dri := dri + a value size ].
			sb := dstPara characterBlockForIndex: i.
			eb := dstPara characterBlockForIndex: dri - 1.
			self
				addHighlightsFrom: sb
				to: eb
				to: d
				color: self removalHighlightColor ] ]
]

{ #category : 'initialization' }
UnifiedDiffJoinSection >> initialize [

	super initialize.
	self copy: JoinSide new
]
