Class {
	#name : #GLMFlexiblePresentationTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMFlexiblePresentationTest >> testDefault [
	| browser |
	browser := GLMTabulator new.
	browser startOn: #(1 2 3).
	self assert: (browser panes first presentations first isKindOf: GLMFlexiblePresentation).
	browser startOn: 1.
	self assert: (browser panes first presentations first isKindOf: GLMFlexiblePresentation).
]

{ #category : #tests }
GLMFlexiblePresentationTest >> testDynamic [
	| browser childBrowser |
	childBrowser := GLMFinder new.
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit
		to: #one;
		andShow: [ :a | a custom: (GLMDynamicPresentation new display: [ childBrowser ]) ].
	browser startOn: 42.
	self assert: browser panes first presentations first currentPresentation first identicalTo: childBrowser.
	self assert: childBrowser pane identicalTo: browser panes first
]
