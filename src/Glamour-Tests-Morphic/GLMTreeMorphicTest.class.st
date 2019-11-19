Class {
	#name : #GLMTreeMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #tests }
GLMTreeMorphicTest >> testChildrenBlock [
	| browser check |
	browser := GLMFinder new.
	check := false.
	browser show: [:a | a tree
		children: [ :each | 
			check := true.
			#() ]].
	window := browser openOn: ($a to: $d).

	self waitUntilUIRedrawed.
	self assert: check equals: true
]

{ #category : #tests }
GLMTreeMorphicTest >> testChildrenBlockTakesEntity [
	| browser receivedEntity |
	browser := GLMFinder new.
	receivedEntity := nil.
	browser show: [:a | a tree
		children: [:each :entity | receivedEntity := entity. #()]].
	window := browser openOn: ($a to: $d).

	self waitUntilUIRedrawed.

	self assert: receivedEntity equals: ($a to: $d).
]

{ #category : #tests }
GLMTreeMorphicTest >> testExplicitSelectionDoesNotUpdate [
	| composite |
	composite := GLMCompositePresentation new
		with: [ :c | c tree 
			allowsRecursiveSearch: false;
			children: [ :each | each  to: each + 10  ] ].
	window := composite openOn: { 1 }.
	
	composite selection: Object new.

	self assert:  self tree selectedMorph equals: nil
]

{ #category : #tests }
GLMTreeMorphicTest >> testTreeAllowDeseletion [
	| browser treeNodeMorph |	
	browser := GLMFinder new.
	browser show: [:a | a list allowDeselection].
	window := browser openOn: ($a to: $d).
	treeNodeMorph := self treeVisibleItems first.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self assert: (browser panes first port: #selection) value isNil
]

{ #category : #tests }
GLMTreeMorphicTest >> testTreeDefaultAllowDeseletion [
	| browser treeNodeMorph |
	browser := GLMFinder new.
	browser show: [:a | a tree].
	window := browser openOn: ($a to: $d).
	treeNodeMorph := self treeVisibleItems first.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self assert: (browser panes first port: #selection) value equals: $a
]

{ #category : #tests }
GLMTreeMorphicTest >> testTreeMultipleSelection [
	| browser treeNodeMorph lastTreeNodeMorph |
	browser := GLMFinder new.
	browser show: [:a | a tree beMultiple].
	window := browser openOn: ($a to: $d).
	treeNodeMorph := self treeVisibleItems first.
	lastTreeNodeMorph := self treeVisibleItems last.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self send: self tree mouseUpWithShift: true onItem: lastTreeNodeMorph.
	self assert: (browser panes first port: #selection) value notNil.
	self assert: (browser panes first port: #selection) value asSet equals: ($a to: $d) asSet
]

{ #category : #tests }
GLMTreeMorphicTest >> testTreeMultipleSelectionWithCancellation [
	| browser treeNodeMorph lastTreeNodeMorph |
	browser := GLMFinder new.
	browser show: [:a | a tree beMultiple allowDeselection].
	window := browser openOn: ($a to: $d).
	treeNodeMorph := self treeVisibleItems first.
	lastTreeNodeMorph := self treeVisibleItems last.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self send: self tree mouseUpWithShift: true onItem: lastTreeNodeMorph.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self assert: (browser panes first port: #selection) value isNil
]

{ #category : #tests }
GLMTreeMorphicTest >> testTreeSelection [
	| browser treeNodeMorph |
	browser := GLMFinder new.
	browser show: [:a | a tree].
	window := browser openOn: ($a to: $d).
	treeNodeMorph := self treeVisibleItems first.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self assert: (browser panes first port: #selection) value equals: $a
]

{ #category : #tests }
GLMTreeMorphicTest >> testTreeSelectionCancellation [
	| browser treeNodeMorph |
	browser := GLMFinder new.
	browser show: [:a | a tree allowDeselection].
	window := browser openOn: ($a to: $d).
	treeNodeMorph := self treeVisibleItems first.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self send: self tree mouseUpOnItem: treeNodeMorph.
	self assert: (browser panes first port: #selection) value isNil
]

{ #category : #tests }
GLMTreeMorphicTest >> testTreeWithDisplay [
	| browser treeCollection |
	treeCollection := {(#root1 -> {(#child1 -> {}) . (#child2 -> {})}) . (#root2 -> {})}.
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | 
			a tree
				display: [ :each | each value ];
				children: [ :x | x value ] ].
	window := browser openOn: #something -> treeCollection.
	self assert: browser panes first presentations first displayValue identicalTo: treeCollection
]

{ #category : #tests }
GLMTreeMorphicTest >> testTreeWithTags [
	"Creates a tree and tag some nodes. Then click on a tag and see if the unmatched nodes disappear"

	| browser model oddNode lineWithNumber3 someNode secondSome |
	model := Dictionary new.
	model at: #even put: #(2 6 8).
	model at: #odd put: #(3 7 9).
	model at: #some put: #(1 2 3 4).
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | 
					(a tree)
						display: [ model keys ];
						sortBlock: [ :x :y | x <= y ];
						children: [ :key | 
									key isNumber
										ifFalse: [ model at: key ] ];
						tags: [ :item | 
									item isNumber
										ifTrue: [ 
											item even
												ifTrue: [ #('even') ]
												ifFalse: [ #('odd') ] ]
										ifFalse: [ #() ] ] ].
	window := browser openOn: model.

	self waitUntilUIRedrawed.

	oddNode := self treeVisibleItems second.	
	
	oddNode toggleExpandedState. "Simulate a click on the node's arrow to expand the node and make sub nodes visible"

	self waitUntilUIRedrawed.

	lineWithNumber3 := self treeVisibleItems third.
	(self find: SimpleButtonMorph in: lineWithNumber3) doButtonAction. "Simulate a click on the 'odd' tag to make even numbers disappear"
	self waitUntilUIRedrawed.

	someNode := self treeVisibleItems fifth.
	someNode toggleExpandedState.

	self waitUntilUIRedrawed.

	secondSome := self find: TextMorph in: self treeVisibleItems seventh.
	self assert: secondSome text asString equals: '3'
]

{ #category : #private }
GLMTreeMorphicTest >> tree [
	^ self find: MorphTreeMorph in: window
]

{ #category : #private }
GLMTreeMorphicTest >> treeVisibleItems [
	"Returns the roots of the tree included in the current window. It looks like morphic creates new collections when the tree is refreshed which means different calls to this method may return different objects."
	^ self tree scroller submorphs
]
