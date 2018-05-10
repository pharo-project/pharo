Class {
	#name : #WindowAnnouncementTest,
	#superclass : #TestCase,
	#instVars : [
		'window'
	],
	#category : #'Morphic-Tests-WindowNotification'
}

{ #category : #running }
WindowAnnouncementTest >> tearDown [ 
	window ifNotNil: [ window delete ].
	super tearDown.

]

{ #category : #'tests - window change' }
WindowAnnouncementTest >> testCollapsing [
	| t |
	window := SystemWindow labelled: 'foo'.
	t := 0.
	window openInWorld.
	window announcer when: WindowCollapsed do: [:ann | t := t + 1].
	self assert: (t = 0).
	window collapse.
	self assert: (t = 1).

]

{ #category : #'tests - window change' }
WindowAnnouncementTest >> testMoving [
	| t oldBounds  event |
	window := SystemWindow labelled: 'foo'.
	t := 0.
	event := nil.

	window openInWorld.
	oldBounds := window bounds.
	window announcer when: WindowMoved do: [:ann | t := t + 1. event := ann].

	self assert: (t = 0).
	self assert: (event isNil).
	
	"We move the window"
	window position: 50@50.
	self assert: (t = 1).
	self assert: (event oldPosition = oldBounds origin).
	self assert: (event newPosition = (50@50 )).

	"We call position: without moving it actually, nothing should happen"
	window position: 50@50.
	self assert: (t = 1).

	"If we simply resize the window, nothing should happen as well"
	window extent: 50@60.
	self assert: (t = 1).


]

{ #category : #'tests - window change' }
WindowAnnouncementTest >> testResizing [
	| t oldBounds newBounds |
	window := SystemWindow labelled: 'foo'.
	window setProperty: #minimumExtent toValue: 1@1.
	t := 0.
	window openInWorld.
	oldBounds := window bounds.
	window announcer when: WindowResizing do: [:ann | t := t + 1].
	self assert: (t = 0).
	window extent: 50@60.
	newBounds := window bounds.
	self assert: (t = 1).

]

{ #category : #'tests - window creation and deletion' }
WindowAnnouncementTest >> testResizingClosing [
	| coll |

	window := SystemWindow labelled: 'foo'.
	coll := OrderedCollection new.
	window openInWorld.
	window announcer when: WindowAnnouncement do: [:ann | coll add: ann].
	self assert: (coll isEmpty).
	window minimizeOrRestore.
	
	"Resizing, moving, deActivation, collapsing "
	self assert: (coll size = 4).
	self assert: (coll first isResized).
	self assert: (coll second isMoved).
	self assert: (coll third isDeActivated).
	self assert: (coll fourth isCollapsed).

	window delete.

	self assert: (coll size = 5).
	self assert: (coll fifth isClosed).
	window := nil
]

{ #category : #'tests - window change' }
WindowAnnouncementTest >> testScrolling [
	"This test tests the scrolling values of a scrollpane. We create a reeeeeally big (enormous) morph, to be sure its size is bigger than the screen and therefore its enclosing window has scrollbars.
	When screen sizes become bigger, you should increase the value, or fix morphic, what happens first :).
	"
	| myMorph pane t |
	window := SystemWindow labelled: 'foo'.
	window extent: 300@200.
	myMorph := Morph new.
	myMorph extent: 10000000000@1000000000.
	pane := ScrollPane new.
	pane scroller addMorph: myMorph.
	window
		addMorph: pane
		fullFrame: LayoutFrame identity.
	t := 0 @ 0.
	window openInWorld.
	window announcer when: WindowScrolling do: [:ann | t := t + ann step ].
	pane hScrollBarValue: 10.
	pane vScrollBarValue: 5.

	window delete.
	
	self assert: (t = (10 @ 5)).
	window := nil
]

{ #category : #'tests - window creation and deletion' }
WindowAnnouncementTest >> testWindowCreation [
	| t oldBounds newBounds |
	t := 0.
	World announcer when: WindowResizing do: [:ann | t := t + 1].
	window := SystemWindow labelled: 'foo'.
	window setProperty: #minimumExtent toValue: 1@1.
	window openInWorld.
	oldBounds := window bounds.
	window announcer when: WindowResizing do: [:ann | t := t + 1].
	self assert: (t = 0).
	window extent: 50@60.
	newBounds := window bounds.
	self assert: (t = 1).

]

{ #category : #'tests - window creation and deletion' }
WindowAnnouncementTest >> testWindowCreationAndDeletion [
	
	| t newWindowCreated |
	t := 0.
	World announcer when: WindowOpened do: [:ann | t := t + 1. newWindowCreated := ann window].
	World announcer when: WindowClosed do: [:ann | t := t + 10. newWindowCreated := ann window].
	window := SystemWindow labelled: 'foo'.
	window openInWorld.

	self assert: (t = 1).
	self assert: (window == newWindowCreated).
	window delete.

	self assert: (t = 11).
	self assert: (window == newWindowCreated).

]

{ #category : #'tests - window creation and deletion' }
WindowAnnouncementTest >> testWindowLabelling [
	"Test change of label for a window."
	
	| labels win |
	labels := #().
	World announcer when: WindowLabelled do: [:ann | win := ann window. labels := labels copyWith: ann label].
	window := SystemWindow labelled: 'foo'.
	window openInWorld.
	self assert: win = window.
	self assert: labels = #('foo').
	window setLabel: 'bar'.
	self assert: win = window.
	self assert: labels = #('foo' 'bar')
]
