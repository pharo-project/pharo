Class {
	#name : #GLMPaneTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMPaneTest >> testAllRecursivePresentations [
	| browser |
	browser := GLMTabulator new.
	browser column: #one.
	(browser transmit)
		to: #one;
		andShow: [ :a | a text ].
	browser startOn: 42.
	self assert: browser panes first withAllPresentations size equals: 2.
	self assert: browser pane withAllPresentations size equals: 4
]

{ #category : #tests }
GLMPaneTest >> testCachedMatchingPresentations [
	| aPane cachedMatchingPresentations |
	aPane := GLMPane in: nil.
	cachedMatchingPresentations := aPane cachedMatchingPresentations.
	self assert: cachedMatchingPresentations identicalTo: aPane cachedMatchingPresentations.
	aPane matchingPresentations.	"this should refresh cached matching presentations"
	self assert: cachedMatchingPresentations ~~ aPane cachedMatchingPresentations
]

{ #category : #tests }
GLMPaneTest >> testCopyAnnouncements [
	| pane newPane |
	pane := GLMPane new.
	pane when: GLMPresentationsChanged do: [ :ann | self fail ].
	self assert: pane numberOfSubscriptions equals: 1.
	newPane := pane copy.
	newPane announce: GLMPresentationsChanged.
	self assert: newPane numberOfSubscriptions equals: 0
]

{ #category : #tests }
GLMPaneTest >> testCopyAnnouncementsDoNotRaise [
	| pane newPane |
	pane := GLMPane new.
	pane when: GLMPresentationsChanged do: [ :ann | self fail ].
	self assert: pane numberOfSubscriptions equals: 1.
	newPane := pane copy.
	newPane announce: GLMPresentationsChanged.
	self assert: newPane numberOfSubscriptions equals: 0
]

{ #category : #tests }
GLMPaneTest >> testCopyNotTheSameRegistry [

	| pane newPane |
	pane := GLMPane new.
	newPane := pane copy.
	self assert: pane glmSubscriptions ~~ newPane glmSubscriptions
]

{ #category : #tests }
GLMPaneTest >> testCreation [
	| aPane |
	aPane := GLMPane named: 1 in: 2.
	self assert: aPane name equals: 1.
	self assert: aPane browser equals: 2
]

{ #category : #tests }
GLMPaneTest >> testDefaultCompositePresentation [

 	| pane |
	pane := GLMPane named: 'test'. 
	self assert: (pane presentations isKindOf: GLMCompositePresentation)
]

{ #category : #tests }
GLMPaneTest >> testDefaultPortValue [
	| pane |
	pane := GLMPane named: 'test'.
	self assert: (pane port: #somePort) value isNil
]

{ #category : #tests }
GLMPaneTest >> testDefaultPresentations [
	| pane |
	pane := GLMPane named: 'test'.
	self assert: (pane presentations isKindOf: GLMCompositePresentation)
]

{ #category : #tests }
GLMPaneTest >> testDefaultValidate [
	| pane |
	pane := GLMPane named: 'test'. 
	self assert: ((pane port: #selection) validate: 5)

]

{ #category : #tests }
GLMPaneTest >> testIn [
	| aPane |
	aPane := GLMPane in: 1.
	self assert: aPane name equals: aPane defaultName.
	self assert: aPane browser equals: 1.
	self assertEmpty: aPane ports.
	self assertEmpty: aPane presentations
]

{ #category : #tests }
GLMPaneTest >> testNamed [
	| aPane |
	aPane := GLMPane named: 1.
	self assert: aPane name equals: 1.
	self assert: aPane browser equals: GLMNoBrowser new.
	self assertEmpty: aPane ports.
	self assert: aPane presentations isEmpty
]

{ #category : #tests }
GLMPaneTest >> testNamedIn [
	| aPane |
	aPane := GLMPane named: 1 in: 2.
	self assert: aPane name equals: 1.
	self assert: aPane browser equals: 2.
	self assertEmpty: aPane ports.
	self assert: aPane presentations isEmpty
]

{ #category : #tests }
GLMPaneTest >> testPorts [
	| pane port |
	pane := GLMPane named: 'test'.
	self assertEmpty: pane ports.
	port := pane port: #foo.
	self assert: pane ports size equals: 1.
	self assert: pane ports first identicalTo: port.
	self assert: (pane port: #foo) identicalTo: port
]

{ #category : #tests }
GLMPaneTest >> testPortsInmmutability [
	<expectedFailure>
	| aPane ports |
	aPane := GLMPane in: 1.
	self assertEmpty: aPane ports.
	ports := aPane ports.
	ports add: 1.
	self assert: ports size equals: 1.
	self assertEmpty: aPane ports
]

{ #category : #tests }
GLMPaneTest >> testPresentationAndBrowserConditions [
	| pane presentation1 presentation2 presentation3 |
	pane := GLMPane named: 'test'.
	presentation1 := GLMExplicitBrowser new condition: [ :first :second | first = 1 and: [ second = 2 ] ].
	presentation2 := GLMTabulator new condition: [ :first | first = #foo ].
	presentation3 := GLMExplicitBrowser new useExplicitNotNil.
	pane addPresentation: presentation1.
	pane addPresentation: presentation2.
	pane addPresentation: presentation3.
	(pane port: #entity) value: (GLMMultiValue withAll: #(1 2)).
	self assert: pane matchingPresentations size equals: 2.
	self assert: pane matchingPresentations first identicalTo: presentation1.
	self assert: pane matchingPresentations last identicalTo: presentation3.
	(pane port: #entity) value: (GLMMultiValue withAll: #(#foo 2)).
	self assert: pane matchingPresentations size equals: 2.
	self assert: pane matchingPresentations first identicalTo: presentation2.
	self assert: pane matchingPresentations last identicalTo: presentation3.
	(pane port: #entity) value: (GLMMultiValue withAll: #(nil 2)).
	self assert: pane matchingPresentations size equals: 1
]

{ #category : #tests }
GLMPaneTest >> testPresentationConditions [
	| pane presentation1 presentation2 presentation3 |
	pane := GLMPane named: 'test'.
	presentation1 := GLMPresentation new condition: [ :first :second | first = 1 and: [ second = 2 ] ].
	presentation2 := GLMPresentation new condition: [ :first | first = #foo ].
	presentation3 := GLMPresentation new useExplicitNotNil.
	pane addPresentation: presentation1.
	pane addPresentation: presentation2.
	pane addPresentation: presentation3.
	(pane port: #entity) value: (GLMMultiValue withAll: #(1 2)).
	self assert: pane matchingPresentations size equals: 2.
	self assert: pane matchingPresentations first identicalTo: presentation1.
	self assert: pane matchingPresentations last identicalTo: presentation3.
	(pane port: #entity) value: (GLMMultiValue withAll: #(#foo 2)).
	self assert: pane matchingPresentations size equals: 2.
	self assert: pane matchingPresentations first identicalTo: presentation2.
	self assert: pane matchingPresentations last identicalTo: presentation3.
	(pane port: #entity) value: (GLMMultiValue withAll: #(nil 2)).
	self assert: pane matchingPresentations size equals: 1
]

{ #category : #tests }
GLMPaneTest >> testPresentations [
	| pane presentation announcement |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	self assertEmpty: pane presentations.
	announcement := nil.
	pane when: GLMPresentationsChanged do: [ :ann | announcement := ann ].
	pane addPresentation: presentation.
	self assert: pane presentations size equals: 1.
	self assert: pane presentations first identicalTo: presentation.
	self assert: presentation pane identicalTo: pane.
	self assert: announcement notNil
]

{ #category : #tests }
GLMPaneTest >> testPresentationsInmmutability [
	<expectedFailure>
	| aPane presentations |
	aPane := GLMPane in: 1.
	self assertEmpty: aPane presentations.
	presentations := aPane presentations.
	presentations add: 1.
	self assert: presentations size equals: 1.
	self assertEmpty: aPane presentations
]

{ #category : #tests }
GLMPaneTest >> testSimplePresentations [
	| pane presentation announcement |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	self assertEmpty: pane presentations.
	announcement := nil.
	pane when: GLMPresentationsChanged do: [ :ann | announcement := ann ].
	pane addPresentation: presentation.
	self assert: pane presentations size equals: 1.
	self assert: pane presentations first identicalTo: presentation.
	self assert: presentation pane identicalTo: pane.
	self assert: announcement notNil
]

{ #category : #tests }
GLMPaneTest >> testValidate [
	| pane presentation |
	pane := GLMPane named: 'test'. 
	presentation := GLMListPresentation new.
	pane addPresentation: presentation.
	presentation entity: (OrderedCollection with: 1 with: 2).
	presentation shouldValidate: true.
	self assert: ((pane port: #selection) validate: 1).
	self assert: ((pane port: #selection) validate: 2).
	self deny: ((pane port: #selection) validate: 5)

]
