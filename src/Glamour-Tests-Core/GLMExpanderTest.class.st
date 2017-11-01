Class {
	#name : #GLMExpanderTest,
	#superclass : #TestCase,
	#category : 'Glamour-Tests-Core'
}

{ #category : #tests }
GLMExpanderTest >> testAllowNilConditionWhenEmbedded [
	| expander tabulator |
	expander := GLMExpander new.
	expander allowNil.
	expander display: [ :x :y | x + y ].	"should lead to an error because nil is allowed in the block"
	expander show: [ :a | a text display: [ :x :y | x asString , y asString ] ].
	tabulator := GLMTabulator new.
	tabulator
		column: #one;
		column: #two.
	(tabulator transmit)
		to: #two;
		fromOutsidePort: #entity;
		from: #one port: #nilPort;
		andShow: [ :a | a custom: expander ].
	self should: [ tabulator startOn: 1 ] raise: Error
]

{ #category : #tests }
GLMExpanderTest >> testConditionWhenEmbedded [
	| expander tabulator |
	expander := GLMExpander new.
	expander display: [ :x :y | x + y ].
	expander show: [ :a | a text display: [ :x :y | x asString , y asString ] ].
	tabulator := GLMTabulator new.
	tabulator
		column: #one;
		column: #two.
	(tabulator transmit)
		to: #two;
		fromOutsidePort: #entity;
		from: #one port: #nilPort;
		andShow: [ :a | a custom: expander ].
	self shouldnt: [ tabulator startOn: 1 ] raise: Error
]

{ #category : #tests }
GLMExpanderTest >> testCreation [
	self assert: GLMExpander new panes isEmpty
]

{ #category : #tests }
GLMExpanderTest >> testPresentations [
	| expander |
	expander := GLMExpander new.
	expander show: [ :a | a text ].
	expander startOn: #(#a #b #c).
	self assert: expander panes size equals: 3.
	self assert: expander panes first presentations entity value equals: #a.
	self assert: expander panes first presentations size equals: 1.
	self assert: (expander panes first presentations first isKindOf: GLMRubricTextPresentation)
]

{ #category : #tests }
GLMExpanderTest >> testSimple [
	| expander |
	expander := GLMExpander new.
	expander startOn: #(#a #b #c).
	self assert: expander panes size equals: 3.
	self assert: (expander panes first port: #entity) value equals: #a
]
