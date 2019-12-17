Class {
	#name : #GLMScriptingTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMScriptingTest >> testActionDefinition [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser
		transmit to: #one;
		andShow: [ :a | 
					a text act: [  ] entitled: ''.
					a list act: [  ] entitled: ''.
					a tree act: [  ] entitled: '' ].
	browser startOn: #(42).
	browser panes first presentations do: [ :each | self assert: each actions size equals: 1 ]
]

{ #category : #tests }
GLMScriptingTest >> testAllowNil [
	| browser presentationsChanged matchingPresentationsChanged |
	browser := GLMTabulator new.
	browser
		column: [ :c | 
					c
						row: #one;
						row: #two ];
		column: #details.
	browser
		transmit to: #one;
		andShow: [ :a | a tree display: #(1 2 3) ].
	browser
		transmit to: #two;
		andShow: [ :a | a tree display: #(#a #b #c) ].
	browser
		transmit to: #details;
		from: #one;
		from: #two;
		andShow: [ :a | 
					(a text)
						display: [ :one :two | one asString , two asString ];
						allowNil ].
	browser startOn: 42.
	(browser paneNamed: #details) when: GLMPresentationsChanged do: [ :a | presentationsChanged := true ].
	(browser paneNamed: #details) when: GLMMatchingPresentationsChanged do: [ :a | matchingPresentationsChanged := true ].
	presentationsChanged := false.
	matchingPresentationsChanged := false.
	((browser paneNamed: #one) port: #selection) value: 2.
	self assert: presentationsChanged.
	self assert: matchingPresentationsChanged.
	self assert: (browser paneNamed: #details) presentations first displayValue equals: '2nil'.
	presentationsChanged := false.
	((browser paneNamed: #two) port: #selection) value: #c.
	self assert: presentationsChanged.
	self assert: matchingPresentationsChanged.
	self assert: (browser paneNamed: #details) presentations first displayValue equals: '2c'.
	presentationsChanged := false.
	((browser paneNamed: #one) port: #selection) value: nil.
	self assert: presentationsChanged.
	self assert: matchingPresentationsChanged.
	self assert: (browser paneNamed: #details) presentations first displayValue equals: 'nilc'
]

{ #category : #tests }
GLMScriptingTest >> testConditionalPresentation [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser
		transmit to: #two;
		from: #one;
		andShow: [ :a | 
					a text when: [ :x | x = 42 ].
					a list when: [ :x | x > 42 ] ].
	((browser paneNamed: #one) port: #selection) value: 42.
	self assert: (browser paneNamed: #two) matchingPresentations size equals: 1.
	self assert: ((browser paneNamed: #two) matchingPresentations first isKindOf: GLMRubricTextPresentation).
	((browser paneNamed: #one) port: #selection) value: 43.
	self assert: (browser paneNamed: #two) matchingPresentations size equals: 1.
	self assert: ((browser paneNamed: #two) matchingPresentations first isKindOf: GLMListPresentation)
]

{ #category : #tests }
GLMScriptingTest >> testDynamicActions [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser
		transmit to: #one;
		andShow: [ :a | 
					(a list)
						display: [ :x | 1 to: x ];
						dynamicActions: [ :list | 
									(1 to: list selection)
										collect: [ :index | 
											(GLMGenericAction new)
												action: [ index ];
												title: 'Action' , index printString;
												yourself ] ] ].
	browser startOn: 42.
	(browser panes first port: #selection) value: 3.
	self assert: browser panes first presentations first dynamicActions size equals: 3.
	(browser panes first port: #selection) value: 5.
	self assert: browser panes first presentations first dynamicActions size equals: 5
]

{ #category : #tests }
GLMScriptingTest >> testEntityInUpdateAction [
	| browser inputFromActionBlock |
	browser := GLMTabulator new.
	browser column: #one.
	browser
		transmit to: #one;
		andShow: [ :a | a text populate: #foo on: $m with: [ :list :input | inputFromActionBlock := input ] ].
	browser startOn: 42.
	browser panes first presentations first actions first actOn: browser panes first presentations first.
	self assert: inputFromActionBlock equals: 42
]

{ #category : #tests }
GLMScriptingTest >> testFinderWithPreviousPort [
	<expectedFailure>
	| browser |
	browser := GLMFinder new.
	browser show: [:a | 
		a list
			title: 'Presentation1';
			display: [ :each | 1 to: each ]].
	browser 
		fromPrevious: #selection;
		passivelyFromPrevious: #highlight;
		andShow: [ :a | 
			a list
				title: 'Presentation2';
				display: [ :selection :highlight | 1 to: selection + highlight ] ].
	browser startOn: 42.
	self assert: browser panes first presentations size equals: 1.
	self assert: browser presentations first title equals: 'Presentation1'.
	self assert: browser transmissions size equals: 1.
	self assert: browser transmissions last origins size equals: 2.
	self assert: browser transmissions last transmissionStrategy presentations size equals: 1.
	self assert: browser transmissions last transmissionStrategy presentations first title equals: 'Presentation2'.
	self assert: (browser panes first port: #entity) value equals: 42.
	(browser panes first port: #highlight) value: 43.
	(browser panes first port: #selection) value: 41.
	self assert: browser panes size equals: 2.
	self assert: browser panes last presentations size equals: 2.
	self assert: browser panes last presentations last displayValue size equals: 84.
	self assert: browser panes last presentations size equals: 2
]

{ #category : #tests }
GLMScriptingTest >> testNestedBrowserEntity [
	| browser innerBrowser realInnerBrowser |
	browser := GLMTabulator new.
	browser column: #one.
	innerBrowser := GLMTabulator new.
	innerBrowser column: #innerOne.
	(innerBrowser transmit)
		to: #innerOne;
		from: #outer port: #entity;
		andShow: [ :a | a text ].
	(browser transmit)
		to: #one;
		andShow: [ :a | (a custom: innerBrowser) display: [ :x | x + 2 ] ].
	browser startOn: 40.
	realInnerBrowser := (browser paneNamed: #one) presentations first.
	self assert: ((browser paneNamed: #one) port: #entity) value equals: 40.
	self assert: (realInnerBrowser pane port: #entity) value equals: 40.
	self assert: ((realInnerBrowser paneNamed: #innerOne) port: #entity) value equals: 42
]

{ #category : #tests }
GLMScriptingTest >> testNestedBrowserOther [
	| browser innerBrowser realInnerBrowser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	innerBrowser := GLMTabulator new.
	innerBrowser column: #innerOne.
	innerBrowser
		transmit to: #innerOne;
		andShow: [ :a | a text ].
	innerBrowser transmit toOutsidePort: #selection; from: #innerOne.
	browser
		transmit to: #one;
		andShow: [ :a | (a custom: innerBrowser) display: [ :x | x + 2 ] ].
	browser
		transmit to: #two;
		from: #one;
		andShow: [ :a | a text ].
	browser startOn: 40.
	realInnerBrowser := (browser paneNamed: #one) presentations first.
	self assert: ((browser paneNamed: #one) port: #entity) value equals: 40.
	self assert: (realInnerBrowser pane port: #entity) value equals: 40.
	self assert: ((realInnerBrowser paneNamed: #innerOne) port: #entity) value equals: 42.
	(realInnerBrowser panes first port: #selection) value: 7.
	self assert: (realInnerBrowser pane port: #selection) value equals: 7.
	self assert: (browser panes last port: #entity) value equals: 7
]

{ #category : #tests }
GLMScriptingTest >> testNestedBrowserWithExplicitTransmission [
	| browser innerBrowser realInnerBrowser |
	browser := GLMTabulator new.
	browser column: #one.
	innerBrowser := GLMTabulator new.
	innerBrowser column: #innerOne.
	(innerBrowser transmit)
		to: #innerOne;
		from: #outer port: #entity;
		andShow: [ :a | a text ].
	(browser transmit)
		to: #one;
		andShow: [ :a | a custom: innerBrowser ].
	browser startOn: 42.
	realInnerBrowser := (browser paneNamed: #one) presentations first.
	self assert: ((browser paneNamed: #one) port: #entity) value equals: 42.
	self assert: ((realInnerBrowser paneNamed: #innerOne) port: #entity) value equals: 42
]

{ #category : #tests }
GLMScriptingTest >> testOuterEntity [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser
		transmit to: #one;
		andShow: [ :a | a text ].
	browser
		transmit to: #two;
		fromOutsidePort: #entity;
		andShow: [ :a | a text ].
	browser startOn: 42.
	self assert: browser transmissions first origins anyOne name equals: #entity.
	self assert: browser transmissions last origins anyOne name equals: #entity.
	self assert: browser transmissions size equals: 2
]

{ #category : #tests }
GLMScriptingTest >> testPublishing [
	| browser |
	browser := GLMTabulator new.
	browser column: #classes.
	browser sendToOutside: #selectedClass from: #classes -> #selection.
	browser sendTo: #classes -> #highlight fromOutside: #highlightedClass.
	(browser panes first port: #selection) value: 42.
	self assert: (browser pane port: #selectedClass) value equals: 42.
	self assert: (browser panes first port: #selection) value equals: 42.
	(browser pane port: #highlightedClass) value: 43.
	self assert: (browser pane port: #highlightedClass) value equals: 43.
	self assert: (browser panes first port: #highlight) value equals: 43.
	(browser panes first port: #highlight) value: 44.
	self assert: (browser pane port: #highlightedClass) value equals: 43.
	self assert: (browser panes first port: #highlight) value equals: 44.
	(browser pane port: #selectedClass) value: 45.
	self assert: (browser pane port: #selectedClass) value equals: 45.
	self assert: (browser panes first port: #selection) value equals: 42
]

{ #category : #tests }
GLMScriptingTest >> testPublishingToOutsideAndInsideFinder [
	| innerBrowser finder realInnerBrowser |
	finder := GLMFinder new.
	innerBrowser := GLMTabulator new.
	innerBrowser
		column: #one;
		column: #two.
	innerBrowser transmit
		to: #one;
		andShow: [ :a | a list display: [ :x | 1 to: 43 ] ].
	innerBrowser transmit
		to: #two;
		from: #one;
		andShow: [ :a | a text ].
	innerBrowser transmit
		toOutsidePort: #selection;
		from: #one port: #selection.
	finder show: [ :a | a custom: innerBrowser ].
	self assert: innerBrowser transmissions size equals: 3.
	self assert: innerBrowser transmissions second origins first pane identicalTo: innerBrowser panes first.
	self assert: innerBrowser transmissions second destination pane identicalTo: innerBrowser panes last.
	finder startOn: 43.
	realInnerBrowser := innerBrowser.
	self assert: realInnerBrowser transmissions size equals: 3.
	self assert: realInnerBrowser transmissions second origins first pane identicalTo: realInnerBrowser panes first.
	self assert: realInnerBrowser transmissions second destination pane identicalTo: realInnerBrowser panes last
]

{ #category : #tests }
GLMScriptingTest >> testPublishingToOutsideFinder [
	| innerBrowser finder |
	finder := GLMFinder new.
	innerBrowser := GLMTabulator new.
	innerBrowser column: #any.
	innerBrowser
		transmit to: #any;
		andShow: [ :a | a list display: [ :x | 1 to: 43 ] ].
	innerBrowser transmit toOutsidePort: #selection; from: #any port: #selection.
	finder show: [:a | a custom: innerBrowser].
	finder startOn: 43.
	self assert: (finder panes first port: #entity) value equals: 43.
	self assert: (finder panes first port: #selection) value isNil.
	((finder panes first presentations first paneNamed: #any) port: #selection) value: 42.
	self assert: (finder panes first port: #selection) value equals: 42.
	self assert: (finder panes first port: #entity) value equals: 43.
	self assert: finder panes size equals: 2.
	self assert: (finder panes last port: #entity) value equals: 42.
	self assert: (finder panes first presentations first isKindOf: GLMTabulator).
	self assert: (finder panes last presentations first isKindOf: GLMTabulator).
	self assert: (finder panes first presentations first isKindOf: GLMTabulator)
]

{ #category : #tests }
GLMScriptingTest >> testReferences [
	| browser pane1 |
	browser := GLMExplicitBrowser new.
	pane1 := browser addNewPaneNamed: #pane1.
	self assert: (browser resolveOriginPort: #pane1) identicalTo: (pane1 port: #selection).
	self assert: (browser resolveOriginPort: #pane1 -> #foo) identicalTo: (pane1 port: #foo).
	self assert: (browser resolveDestinationPort: #pane1) identicalTo: (pane1 port: #entity).
	self assert: (browser resolveDestinationPort: #pane1 -> #foo) identicalTo: (pane1 port: #foo).
	self assert: ((browser resolveOriginPort: #outer) isKindOf: GLMPresentationBoundPort).
	self assert: (browser resolveOriginPort: #outer) presentation identicalTo: browser.
	self assert: (browser resolveOriginPort: #outer) name equals: #selection.
	self assert: ((browser resolveOriginPort: #outer -> #foo) isKindOf: GLMPresentationBoundPort).
	self assert: (browser resolveOriginPort: #outer -> #foo) presentation identicalTo: browser.
	self assert: (browser resolveOriginPort: #outer -> #foo) name equals: #foo.
	self assert: ((browser resolveDestinationPort: #outer) isKindOf: GLMPresentationBoundPort).
	self assert: (browser resolveDestinationPort: #outer) presentation identicalTo: browser.
	self assert: (browser resolveDestinationPort: #outer) name equals: #entity.
	self assert: ((browser resolveDestinationPort: #outer -> #foo) isKindOf: GLMPresentationBoundPort).
	self assert: (browser resolveDestinationPort: #outer -> #foo) presentation identicalTo: browser.
	self assert: (browser resolveDestinationPort: #outer -> #foo) name equals: #foo
]

{ #category : #tests }
GLMScriptingTest >> testScriptingDefaults [
	| browser |
	browser := GLMTabulator new.
	browser startOn: 42.
	self assert: browser panes size equals: 1.
	self assert: browser transmissions size equals: 1.
	self assert: browser transmissions first origins first equals: (browser pane port: #entity).
	self assert: browser transmissions first destination equals: (browser panes first port: #entity)
]

{ #category : #tests }
GLMScriptingTest >> testSimpleTransmission [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser sendTo: #two -> #port2 from: #one -> #port1.
	self assert: browser transmissions size equals: 1.
	self assert: (browser transmissions first transmissionStrategy isKindOf: GLMNoStrategy).
	self assert: browser transmissions first origins first equals: ((browser paneNamed: #one) port: #port1).
	self assert: browser transmissions first destination equals: ((browser paneNamed: #two) port: #port2)
]

{ #category : #tests }
GLMScriptingTest >> testSimpleTransmissionsToOuter [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	browser sendTo: #outer -> #increased from: #outer -> #entity with: [ :x | x + 1 ].
	(browser transmit)
		to: #one;
		from: #outer port: #increased;
		andShow: [ :a | a text ].
	browser startOn: 41.
	self assert: ((browser paneNamed: #one) port: #entity) value equals: 42
]

{ #category : #tests }
GLMScriptingTest >> testStartTransmission [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser transmit
		to: #one;
		andShow: [ :a | a list ].
	self assert: browser transmissions size equals: 1.
	self assertEmpty: (browser paneNamed: #one) presentations.
	self assertEmpty: (browser paneNamed: #two) presentations.
	browser startOn: 15.
	self assert: (browser paneNamed: #one) presentations size equals: 1.
	self assertEmpty: (browser paneNamed: #two) presentations
]

{ #category : #tests }
GLMScriptingTest >> testStartTransmissionFromInitializationBlock [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser transmit
		to: #one;
		andShow: [ :a | a list initialize: [ :pres | pres selection: 4 ] ].
	browser transmit
		from: #one port: #selection;
		to: #two port: #entity;
		andShow: [ :a | a text ].
	self assertEmpty: (browser paneNamed: #one) presentations.
	self assertEmpty: (browser paneNamed: #two) presentations.
	browser startOn: 42.
	self assert: (browser paneNamed: #one) presentations size equals: 1.
	self assert: (browser paneNamed: #one) presentations first selection equals: 4.
	self assert: (browser paneNamed: #two) presentations size equals: 1
]

{ #category : #tests }
GLMScriptingTest >> testStartWithOnlyJumpStart [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser transmit
		to: #one;
		andShow: [ :a | a list ].
	browser transmit
		to: #two;
		from: #one;
		andShow: [ :a | a text ].
	self assert: browser transmissions size equals: 2.
	self assertEmpty: (browser paneNamed: #one) presentations.
	self assertEmpty: (browser paneNamed: #two) presentations.
	browser startOn: #(15 25 35).
	self assert: (browser paneNamed: #one) presentations size equals: 1.
	self assert: (browser paneNamed: #one) matchingPresentations size equals: 1.
	self assertEmpty: (browser paneNamed: #two) presentations.
	self assertEmpty: (browser paneNamed: #two) matchingPresentations
]

{ #category : #tests }
GLMScriptingTest >> testThreePanesTransmissions [
	| browser two |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	(browser transmit)
		to: #three;
		from: #one;
		passivelyFrom: #two;
		andShow: [ :a | a text display: [ :x :y | two := y ] ].
	self assert: browser transmissions size equals: 1.
	self assert: browser transmissions first passiveOrigins size equals: 1.
	self assert: browser transmissions first activeOrigins size equals: 1.
	((browser paneNamed: #two) port: #selection) value: 15.
	self assert: ((browser paneNamed: #two) port: #selection) value equals: 15.
	self assert: ((browser paneNamed: #three) port: #entity) value isNil.
	((browser paneNamed: #one) port: #selection) value: 20.
	self assert: ((browser paneNamed: #one) port: #selection) value equals: 20.
	self assert: ((browser paneNamed: #three) port: #entity) value first equals: 20.
	self assert: ((browser paneNamed: #three) port: #entity) value last equals: 15
]

{ #category : #tests }
GLMScriptingTest >> testTransmissionWithTwoOrigins [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	browser
		transmit to: #three;
		from: #one;
		from: #two;
		andShow: [ :a | a text ].
	((browser paneNamed: #one) port: #selection) value: 42.
	self assert: ((browser paneNamed: #three) port: #entity) value first equals: 42.
	self assert: ((browser paneNamed: #three) port: #entity) value last equals: nil.
	self assert: (browser paneNamed: #three) presentations size equals: 1.
	self assert: (browser paneNamed: #three) matchingPresentations size equals: 0.
	((browser paneNamed: #two) port: #selection) value: 43.
	self assert: ((browser paneNamed: #three) port: #entity) value first equals: 42.
	self assert: ((browser paneNamed: #three) port: #entity) value last equals: 43.
	self assert: (browser paneNamed: #three) matchingPresentations size equals: 1
]

{ #category : #tests }
GLMScriptingTest >> testTwoTransmissionsWithOnlyOneEvent [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two;
		column: #three.
	browser transmit
		to: #two;
		from: #one;
		andShow: [ :a | a list ].
	browser transmit
		to: #three;
		from: #two;
		andShow: [ :a | a text ].
	self assert: browser transmissions size equals: 2.
	self assertEmpty: (browser paneNamed: #one) presentations.
	self assertEmpty: (browser paneNamed: #two) presentations.
	self assertEmpty: (browser paneNamed: #three) presentations.
	((browser paneNamed: #one) port: #selection) value: 42.
	self assert: ((browser paneNamed: #two) port: #entity) value equals: 42.
	self assertEmpty: (browser paneNamed: #one) presentations.
	self assert: (browser paneNamed: #two) presentations size equals: 1.
	self assert: (browser paneNamed: #two) matchingPresentations size equals: 1.
	self assertEmpty: (browser paneNamed: #three) presentations.
	self assertEmpty: (browser paneNamed: #three) matchingPresentations
]

{ #category : #tests }
GLMScriptingTest >> testUpdateAction [
	| browser |
	browser := GLMTabulator new.
	browser
		column: #one;
		column: #two.
	browser transmit
		to: #one;
		andShow: [ :a | a list populate: #foo on: $m with: [ :list :input | 42 ] ].
	browser transmit
		to: #two;
		from: #one port: #foo;
		andShow: [ :a | a text ].
	browser startOn: #(1 2 3).
	self assert: browser panes first presentations size equals: 1.
	self assertEmpty: browser panes last presentations.
	browser panes first presentations first actions first actOn: browser panes first presentations first.
	self assert: browser panes second presentations size equals: 1.
	self assert: browser panes second presentations first entity equals: 42
]

{ #category : #tests }
GLMScriptingTest >> testUpdateYourselfOn [

	| announcer entity browser presentation |
	announcer := Announcer new.
	entity := OrderedCollection with: 1 with: 2 with: 3.
	browser := GLMTabulator new.
	browser column: #one.
	browser transmit to: #one; andShow: [ :a |
		presentation := a list 
			updateOn: GLMTestAnnouncement from: [ announcer ];
			shouldValidate: true ].
	browser startOn: entity.
	presentation registerAnnouncements.
	presentation selection: 3.
	entity removeLast.
	announcer announce: GLMTestAnnouncement.
	self assert: presentation selection isNil 
]
