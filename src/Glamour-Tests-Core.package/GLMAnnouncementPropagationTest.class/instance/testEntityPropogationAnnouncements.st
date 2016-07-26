tests
testEntityPropogationAnnouncements
	"This tests the order in which announcements are raised in nested browsers. 
 
	This is of importance as, in this test, we add a browser to a pane when a 
	certain port of that pane gets updated using a BundleTransmission but this 
	browser has a transmission from its #outer pane that refers to the same 
	port.  In such a case we want the presentation to be set first so that it can 
	be notified when the outer port changes. 
 
	This is a sister test to testEntityPropogation that ensures that the 
	announcements are raised in the right order and not redundantly."

	| browser1 browser2 |
	browser1 := (GLMTabulator new)
		title: '1';
		yourself.
	browser2 := (GLMTabulator new)
		title: '2';
		yourself.
	browser1 pane name: 'b1 parent pane'.
	browser1 column: #'b1-pane'.
	browser2 column: #'b2-pane'.
	self assert: browser1 pane presentations size equals: 1.
	self assert: browser1 pane presentations first == browser1.
	(browser1 transmit)
		to: #'b1-pane';
		andShow: [ :a | a custom: browser2 ].
	(browser2 transmit)
		to: #'b2-pane';
		andShow: [ :a | a text ].
	self assert: GLMLogger instance announcements copy isEmpty.
	browser1 startOn: 42.
	self assert: GLMLogger instance announcements copy size equals: 12.	"1. The outer browser's outer pane changes its value."
	self assert: ((GLMLogger instance announcements copy at: 1) isKindOf: GLMContextChanged).
	self assert: (GLMLogger instance announcements copy at: 1) presentation == browser1.
	self assert: (GLMLogger instance announcements copy at: 1) property equals: #entity.
	self assert: (GLMLogger instance announcements copy at: 1) value equals: 42.	"2. The jumpstart transmission of the outer browser is triggered, updating the presentations..."
	self assert: ((GLMLogger instance announcements copy at: 2) isKindOf: GLMPresentationsChanged).
	self assert: (GLMLogger instance announcements copy at: 2) pane == browser1 panes first.
	self assert: (GLMLogger instance announcements copy at: 2) oldPresentations isEmpty.
	self assert: (GLMLogger instance announcements copy at: 2) presentations size equals: 1.
	self assert: (GLMLogger instance announcements copy at: 2) presentations first equals: browser2.	"3. ...and then the context."
	self assert: ((GLMLogger instance announcements copy at: 3) isKindOf: GLMContextChanged).
	self assert: (GLMLogger instance announcements at: 3) presentation equals: browser2.
	self assert: (GLMLogger instance announcements at: 3) property equals: #entity.
	self assert: (GLMLogger instance announcements at: 3) value equals: 42.	"4. The jumpstart transmission of the inner browser is triggered, updating the presentations..."
	self assert: ((GLMLogger instance announcements at: 4) isKindOf: GLMPresentationsChanged).
	self assert: (GLMLogger instance announcements at: 4) pane == browser2 panes first.
	self assert: (GLMLogger instance announcements at: 4) oldPresentations isEmpty.
	self assert: (GLMLogger instance announcements at: 4) presentations size equals: 1.
	self assert: (GLMLogger instance announcements at: 4) presentations first isNil.	"5. ...and then the context."
	self assert: ((GLMLogger instance announcements at: 5) isKindOf: GLMContextChanged).
	self assert: (GLMLogger instance announcements at: 5) presentation isNil.
	self assert: (GLMLogger instance announcements at: 5) property equals: #entity.
	self assert: (GLMLogger instance announcements at: 5) value equals: 42.	"6. Due to the context change of 5, the innermost presentation now matches."
	self assert: ((GLMLogger instance announcements at: 6) isKindOf: GLMMatchingPresentationsChanged).
	self assert: (GLMLogger instance announcements at: 6) pane == browser2 panes first.
	self assert: (GLMLogger instance announcements at: 6) oldMatchingPresentations isEmpty.
	self assert: (GLMLogger instance announcements at: 6) matchingPresentations size equals: 1.
	self assert: (GLMLogger instance announcements at: 6) matchingPresentations first isNil.	"7. Due to the context change of 3, the inner browser now matches."
	self assert: ((GLMLogger instance announcements at: 7) isKindOf: GLMMatchingPresentationsChanged).
	self assert: (GLMLogger instance announcements at: 7) pane == browser1 panes first.
	self assert: (GLMLogger instance announcements at: 7) oldMatchingPresentations isEmpty.
	self assert: (GLMLogger instance announcements at: 7) matchingPresentations size equals: 1.
	self assert: (GLMLogger instance announcements at: 7) matchingPresentations first == browser2.	"8. Due to the context change of 1, the outer browser now matches."
	self assert: ((GLMLogger instance announcements at: 8) isKindOf: GLMMatchingPresentationsChanged).
	self assert: (GLMLogger instance announcements at: 8) pane == browser1 pane.
	self assert: (GLMLogger instance announcements at: 8) oldMatchingPresentations isEmpty.
	self assert: (GLMLogger instance announcements at: 8) matchingPresentations size equals: 1.
	self assert: (GLMLogger instance announcements at: 8) matchingPresentations first == browser1