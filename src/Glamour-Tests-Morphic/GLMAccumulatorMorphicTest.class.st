Class {
	#name : #GLMAccumulatorMorphicTest,
	#superclass : #GLMMorphicTest,
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #private }
GLMAccumulatorMorphicTest >> tabGroup [
	^ self find: TabGroupMorph in: window
]

{ #category : #tests }
GLMAccumulatorMorphicTest >> testActiveEntity [
	| browser |
	browser := GLMAccumulator new.
	browser show: [ :a | a list display: [ :x | $a to: x ] ].
	browser entity: $d.
	browser entity: $e.
	window := browser openOn: $d.
	browser entity: $c.
	self tabGroup selectedPageIndex: 1.
	self tabGroup update: #selectedIndex.
	self assert: browser activeEntity equals: $d
]

{ #category : #tests }
GLMAccumulatorMorphicTest >> testCloseTab [
	| browser |
	browser := GLMAccumulator new.
	browser
		show: [ :a | 
			a title: [ :x | x asString ].
			a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	browser entity: $c.
	self assert: browser panes size equals: 2.
	self assert: self tabGroup pageMorphs size equals: 2.
	self tabGroup removePageIndex: 1.
	self assert: self tabGroup pageMorphs size equals: 1.
	self assert: browser panes size equals: 1
]

{ #category : #tests }
GLMAccumulatorMorphicTest >> testCreation [
	| browser |
	browser := GLMAccumulator new.
	browser show: [ :a | a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	self find: LazyTabGroupMorph in: window
]

{ #category : #tests }
GLMAccumulatorMorphicTest >> testListInside [
	| browser firstTreeMorph |
	browser := GLMAccumulator new.
	browser show: [ :a | a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	firstTreeMorph := self find: MorphTreeMorph  in: (self tabGroup pageAt: 1).
	self assert: ((firstTreeMorph roots collect: #complexContents) collect: #item) equals: ($a to: $d) asArray
]

{ #category : #tests }
GLMAccumulatorMorphicTest >> testPaneCreation [
	| browser |
	browser := GLMAccumulator new.
	browser show: [ :a | a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	self assert: self tabGroup pages size equals: 1.
	browser entity: $c.
	self assert: self tabGroup pages size equals: 2
]

{ #category : #tests }
GLMAccumulatorMorphicTest >> testPaneRemoval [
	| browser |
	browser := GLMAccumulator new.
	browser show: [ :a | a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	self assert: browser activeEntity equals: $d.
	browser entity: $c.
	self assert: browser activeEntity equals: $c.
	self tabGroup removePageIndex: 1.
	self assert: self tabGroup tabSelectorMorph tabs size equals: 1.
	self assert: browser activeEntity equals: $c.
	self tabGroup removePageIndex: 1.
	self assert: self tabGroup tabSelectorMorph tabs isEmpty.
	self assert: browser activeEntity isNil.
	browser entity: $c.
	self assert: self tabGroup tabSelectorMorph tabs size equals: 1
]

{ #category : #tests }
GLMAccumulatorMorphicTest >> testSelectedTab [
	| browser |
	browser := GLMAccumulator new.
	browser
		show: [ :a | 
			a title: [ :x | x asString ].
			a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	self assert: self tabGroup selectedPageIndex equals: 1.
	browser entity: $c.
	self assert: self tabGroup selectedPageIndex equals: 2.
	browser entity: $d.
	self assert: self tabGroup selectedPageIndex equals: 1
]

{ #category : #tests }
GLMAccumulatorMorphicTest >> testTabTitle [
	| browser firstTabLabel |
	browser := GLMAccumulator new.
	browser
		show: [ :a | 
			a title: [ :x | x asString ].
			a list display: [ :x | $a to: x ] ].
	window := browser openOn: $d.
	firstTabLabel := (self find: GLMTabLabelBrick in: self tabGroup tabSelectorMorph) submorphs first.
	self assert: firstTabLabel submorphs size equals: 2.
	self assert: firstTabLabel submorphs first label text equals: 'd'.
	self find: GLMActionButtonBrick in: firstTabLabel.
]
