Class {
	#name : #GLMFinderTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMFinderTest >> testEmptyPanes [
	| browser | 
	browser := GLMFinder new. 
	self assertEmpty: browser panes. 
	self assertEmpty: browser transmissions.
]

{ #category : #tests }
GLMFinderTest >> testExplicitTransmission [
	| finder |
	finder := GLMFinder new.
	finder transmit fromOutsidePort: #entity; toOutsidePort: #selection.
	finder startOn: 42.
	self assert: (finder pane port: #selection) value equals: 42
]

{ #category : #tests }
GLMFinderTest >> testExplicitTransmissionComplex [
	"this tests a possible bug in the context copying that might prevent the explicit outside transmission"

	| finder browser |
	finder := GLMFinder new.
	finder transmit fromOutsidePort: #entity; toOutsidePort: #something.

	browser := GLMTabulator new.
	browser row: #explorer; row: #details.
	browser transmit to: #explorer; andShow: [:a | a custom: finder].
	browser transmit from: #explorer port: #something; to: #details; andShow: [:a | a text ].
	browser startOn: 42.

	self assert: (browser panes first port: #something) value equals: 42
]

{ #category : #tests }
GLMFinderTest >> testFixedSize [
	| browser | 
	browser := GLMFinder new. 
	self assert: browser hasFixedSizePanes.
	browser hasFixedSizePanes: false.
	self assert: browser hasFixedSizePanes not	
]

{ #category : #tests }
GLMFinderTest >> testLastActivePresentation [
	| browser |
	browser := GLMFinder new.
	browser
		show: [ :a | 
			a list title: 'a'.
			a text title: 'b'.
			a table title: 'c' ].
	browser startOn: 1.
	self assert: browser lastActivePresentation isNil.
	(browser panes first port: #selection) value: 2.
	self assert: browser lastActivePresentation isNil.
	browser panes last lastActivePresentation: browser panes last presentations last.
	self assert: browser lastActivePresentation equals: browser panes last presentations last.
	(browser panes first port: #selection) value: 3.
	self assert: browser panes last lastActivePresentation title equals: 'c'
]

{ #category : #tests }
GLMFinderTest >> testPaneAddedAnnouncement [
	| browser count |
	count := 0.
	browser := GLMFinder new.
	browser when: GLMPaneAdded do: [ :ann | count := count + 1 ].
	browser startOn: #a.
	self assert: count equals: 1.
	(browser panes first port: #selection) value: #b.
	self assert: count equals: 2
]

{ #category : #tests }
GLMFinderTest >> testPaneCreation [
	| browser |
	browser := GLMFinder new.
	browser addPresentation: (GLMPresentation new title: 'presentation1').
	self assert: browser panes isEmpty.	"Outer entity changed."
	(browser pane port: #entity) value: #value1.
	self assert: browser panes size equals: 1.
	self assert: browser panes first browser identicalTo: browser.
	self assert: (browser panes first port: #entity) value equals: #value1.
	self assert: browser panes first presentations size equals: 1.
	self assert: browser panes first presentations first title equals: 'presentation1'.
	self assert: browser panes first presentations first ~~ browser transmission transmissionStrategy presentations first.	"First pane selection changed."
	(browser panes first port: #selection) value: #value2.
	self assert: browser panes size equals: 2.
	self assert: browser panes last browser identicalTo: browser.
	self assert: (browser panes last port: #entity) value equals: #value2.
	self assert: browser panes last presentations first title equals: 'presentation1'.
	self assert: browser panes last presentations first ~~ browser transmission transmissionStrategy presentations first.
	self assert: browser panes last presentations first ~~ browser panes first presentations first.	"Second pane selection changed."
	(browser panes last port: #selection) value: #value3.
	self assert: browser panes size equals: 3.
	self assert: browser panes last browser identicalTo: browser.
	self assert: (browser panes last port: #entity) value equals: #value3.	"First pane selection changed again."
	(browser panes first port: #selection) value: #value4.
	self assert: browser panes size equals: 2.
	self assert: browser panes last browser identicalTo: browser.
	self assert: (browser panes last port: #entity) value equals: #value4.	"Outer entity changed again."
	(browser pane port: #entity) value: #value5.
	self assert: browser panes size equals: 1.
	self assert: browser panes last browser identicalTo: browser.
	self assert: (browser panes last port: #entity) value equals: #value5
]

{ #category : #tests }
GLMFinderTest >> testPaneName [
	| browser |
	browser := GLMFinder new.
	browser addPresentation: (GLMPresentation new title: 'presentation1').
	(browser pane port: #entity) value: #value1.
	self assert: browser panes first name equals: '1'.
	(browser panes first port: #selection) value: #value2.
	self assert: browser panes last name equals: '2'
]

{ #category : #tests }
GLMFinderTest >> testPaneRemovedAnnouncement [
	| browser count |
	count := 0.
	browser := GLMFinder new.
	browser when: GLMPaneRemoved do: [ :ann | count := count + 1 ].
	browser startOn: #a.
	self assert: count equals: 0.
	(browser panes first port: #selection) value: #b.
	self assert: count equals: 0.
	(browser panes second port: #selection) value: #c.
	self assert: count equals: 0.
	(browser panes first port: #selection) value: #d.
	self assert: count equals: 1
]

{ #category : #tests }
GLMFinderTest >> testPaneReplacedAnnouncement [
	| browser count |
	count := 0.
	browser := GLMFinder new.
	browser when: GLMPaneReplaced do: [ :ann | count := count + 1 ].
	browser startOn: #a.
	self assert: count equals: 0.
	(browser panes first port: #selection) value: #b.
	self assert: count equals: 0.
	(browser panes second port: #selection) value: #c.
	self assert: count equals: 0.
	(browser panes first port: #selection) value: #d.
	self assert: count equals: 1.
	browser entity: #e.
	self assert: count equals: 2
]

{ #category : #tests }
GLMFinderTest >> testPanesHaveDifferentRegistries [
	| browser |
	browser := GLMFinder new.
	browser addPresentation: (GLMPresentation new title: 'presentation1').
	self assertEmpty: browser panes.	"Outer entity changed."
	(browser pane port: #entity) value: #value1.
	self assert: browser panes size equals: 1.
	(browser panes first port: #selection) value: #value2.
	self assert: browser panes size equals: 2.
	self deny: (browser panes first instVarNamed: 'registry') identicalTo: (browser panes second instVarNamed: 'registry')
]

{ #category : #tests }
GLMFinderTest >> testPortPublishing [
	| browser |
	browser := GLMFinder new.
	(browser pane port: #entity) value: #value1.
	self assert: browser panes size equals: 1.
	self assert: browser pane ports size equals: 1.
	self assert: (browser pane port: #entity) value equals: #value1.
	(browser panes first port: #selection) value: #value2.
	self assert: browser panes size equals: 2.
	self assert: browser pane ports size equals: 3.
	self assert: (browser pane port: #entity) value equals: #value1.
	self assert: (browser pane port: #selection) value equals: #value2.
	(browser panes last port: #selection) value: #value3.
	self assert: browser panes size equals: 3.
	self assert: browser pane ports size equals: 3.
	self assert: (browser pane port: #entity) value equals: #value1.
	self assert: (browser pane port: #selection) value equals: #value3.
	(browser panes first port: #hover) value: #value4.
	self assert: browser panes size equals: 3.
	self assert: browser pane ports size equals: 4.
	self assert: (browser pane port: #entity) value equals: #value1.
	self assert: (browser pane port: #selection) value equals: #value3.
	self assert: (browser pane port: #hover) value equals: #value4
]

{ #category : #tests }
GLMFinderTest >> testUnregisterAnnouncementsWhenRemovingPane [
	| announcer browser presentation actualFirstPresentation actualLastPresentation |
	announcer := GLMAnnouncer new.
	browser := GLMFinder new.
	presentation := GLMPresentation new.
	presentation title: [ :entity | entity ].
	browser addPresentation: presentation.
	presentation updateOn: GLMTestAnnouncement from: [ announcer ].
	browser entity: #value1.
	actualFirstPresentation := browser panes first presentations first.
	actualFirstPresentation registerAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 1.
	self assert: announcer glmSubscriptions anyOne announcementClass equals: GLMTestAnnouncement.
	self assert: announcer glmSubscriptions anyOne subscriber presentation equals: actualFirstPresentation.
	self assert: announcer glmSubscriptions anyOne subscriber presentation identicalTo: actualFirstPresentation.
	(browser panes first port: #selection) value: #value2.
	browser panes last presentations first registerAnnouncements.
	actualLastPresentation := browser panes last presentations first.
	self assert: announcer numberOfSubscriptions equals: 2.
	self assert: (announcer glmSubscriptions allSatisfy: [ :each | each announcementClass = GLMTestAnnouncement ]).
	self assert: (announcer glmSubscriptions anySatisfy: [ :each | each subscriber presentation = actualFirstPresentation ]).
	self assert: (announcer glmSubscriptions anySatisfy: [ :each | each subscriber presentation == actualFirstPresentation ]).
	(browser panes first port: #selection) value: #value3.
	actualLastPresentation := browser panes last presentations first.	"because we removed the other pane"
	self assert: announcer numberOfSubscriptions equals: 1.
	self assert: announcer glmSubscriptions anyOne announcementClass equals: GLMTestAnnouncement.
	self assert: announcer glmSubscriptions anyOne subscriber presentation equals: actualFirstPresentation.
	self assert: announcer glmSubscriptions anyOne subscriber presentation identicalTo: actualFirstPresentation
]

{ #category : #tests }
GLMFinderTest >> testValidSubscriptionsInComplexBrowser [
	| browser navigator tabulator1 tabulator2 pane1 pane2 content1 content2 details1 details2|
	
	navigator := GLMTabulator new.
	navigator row: #content; row: #details.
	navigator transmit to: #content; andShow: [:a | a list display: #allSuperclasses].
	navigator transmit from: #content; to: #details; andShow: [ :a | a text].
	navigator transmit from: #content; toOutsidePort: #selection.
	 
	browser := GLMFinder new. 
	browser show: [:a | a custom: navigator].
	self assertEmpty: browser panes.
	(browser pane port: #entity) value: String. 
	self assert: browser panes size equals: 1. 
	pane1 := browser panes first.
	tabulator1 := pane1 presentations first.
	content1 := tabulator1 panes first.
	self assert: content1 presentations first displayValue size equals: 5. "5 superclasses of String".
	(content1 port: #selection) value: Collection. "Assumes Collection is a superclass of String" 
	self assert: browser panes size equals: 2. 
	pane2 := browser panes second.
	tabulator2 := pane2 presentations first.
	content2 := tabulator2 panes first.
	self assert: content1 presentations first displayValue size equals: 5. "5 superclasses of String".
	self assert: content2 presentations first displayValue size equals: 2. "2 superclasses of Collection".
	details1 := tabulator1 panes second.
	self assert: details1 presentations first displayValue equals: Collection.
	(content2 port: #selection) value: Object. "Assumes Object is a superclass of Collection"
	self assert: details1 presentations first displayValue equals: Collection.
	details2 := tabulator2 panes second.
	self assert: details2 presentations first displayValue equals: Object.
	self assert: content1 presentations first displayValue size equals: 5. "5 superclasses of String".
	self assert: content2 presentations first displayValue size equals: 2. "2 superclasses of Collection".
]
