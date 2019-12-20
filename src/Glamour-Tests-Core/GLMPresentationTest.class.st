Class {
	#name : #GLMPresentationTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMPresentationTest >> testActions [
	| listing |
	listing := GLMPresentation new.
	listing actions isEmpty.
	listing addAction: GLMAction new.
	listing actions size = 1.
]

{ #category : #tests }
GLMPresentationTest >> testAllActions [
	| listing |
	listing := GLMPresentation new.
	self assert: listing hasActions not.
	listing addAction: GLMAction new.
	listing addAction: GLMAction new.
	listing dynamicActions: [ :p | OrderedCollection with: GLMAction new with: GLMAction new with: GLMAction new ].
	self assert: listing allActions size equals: 5.
	self assert: listing hasActions
]

{ #category : #tests }
GLMPresentationTest >> testAllSelectionActionsWithNoSelection [
	| listing |
	listing := GLMPresentation new.
	listing addSelectionAction: GLMAction new.
	listing addSelectionAction: GLMAction new.
	listing
		dynamicActionsOnSelection: [ :p | OrderedCollection with: GLMAction new with: GLMAction new with: GLMAction new ].
	self assert: listing allSelectionActions size equals: 2
]

{ #category : #tests }
GLMPresentationTest >> testAllSelectionActionsWithSelection [
	| listing |
	listing := GLMPresentation new.
	listing addSelectionAction: GLMAction new.
	listing addSelectionAction: GLMAction new.
	listing selection: Object new.
	listing
		dynamicActionsOnSelection: [ :p | OrderedCollection with: GLMAction new with: GLMAction new with: GLMAction new ].
	self assert: listing allSelectionActions size equals: 5
]

{ #category : #tests }
GLMPresentationTest >> testAnnouncement [
	| pane presentation announcement |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	announcement := nil.
	presentation when: GLMContextChanged do: [ :ann | announcement := ann ].
	(pane port: #entity) value: 42.
	self assert: announcement notNil.
	self assert: announcement property equals: #entity.
	self assert: announcement value equals: 42
]

{ #category : #tests }
GLMPresentationTest >> testContext [
	| pane presentation |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	presentation selection: 42.
	self assert: (pane port: #selection) value equals: 42.
	(pane port: #entity) value: 'foo'.
	self assert: presentation entity equals: 'foo'
]

{ #category : #tests }
GLMPresentationTest >> testCopyAnnouncementsDoNotRaise [
	| presentation newPresentation |
	presentation := GLMPresentation new.
	presentation when: GLMContextChanged do: [ :ann | self fail ].
	self assert: presentation numberOfSubscriptions equals: 1.
	newPresentation := presentation copy.
	newPresentation announce: GLMContextChanged.
	self assert: newPresentation numberOfSubscriptions equals: 0
]

{ #category : #tests }
GLMPresentationTest >> testCopyNotTheSameRegistry [

	| presentation newPresentation |
	presentation := GLMPresentation new.
	newPresentation := presentation copy.
	self assert: presentation glmSubscriptions ~~ newPresentation glmSubscriptions
]

{ #category : #tests }
GLMPresentationTest >> testCopyRootPrototype [
	| presentation newPresentation yetAnotherPresentation |
	presentation := GLMPresentation new.
	newPresentation := presentation copy.
	self assert: newPresentation rootPrototype equals: presentation.
	self assert: newPresentation parentPrototype equals: presentation.
	yetAnotherPresentation := newPresentation copy.
	self assert: yetAnotherPresentation rootPrototype equals: presentation.
	self assert: yetAnotherPresentation parentPrototype equals: newPresentation
]

{ #category : #tests }
GLMPresentationTest >> testCopyTheTransformation [
	| presentation newPresentation |
	presentation := GLMPresentation new.
	presentation selectionTransformation: [:each | each + 1].
	newPresentation := presentation copy.
	self assert: (presentation selectionTransformation value: 1) equals: 2.
	self assert: (newPresentation selectionTransformation value: 1) equals: 2.
]

{ #category : #tests }
GLMPresentationTest >> testDefaultPane [
	| presentation pane |
	presentation := GLMPresentation new.
	pane := presentation pane.
	self assert: pane presentations first pane identicalTo: pane.
	self assert: pane presentations pane identicalTo: pane.
	self assert: pane presentations first identicalTo: presentation
]

{ #category : #tests }
GLMPresentationTest >> testDisplay [
	| presentation |
	presentation := GLMPresentation new.
	presentation entity: #foo.
	self assert: presentation displayValue equals: #foo.
	presentation entity: #(#foo #bar).
	self assert: presentation displayValue equals: #(#foo #bar).
	presentation entity: (GLMMultiValue withAll: #(#foo #bar)).
	self assert: presentation displayValue equals: #foo.
	presentation transformation: [ :first :second | (first , second) asSymbol ].
	self assert: presentation displayValue equals: #foobar
]

{ #category : #tests }
GLMPresentationTest >> testEntityPortInitialization [
	| pane presentation |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new initialize: [ :pres | pres entity: 2 ].
	pane addPresentation: presentation.
	self assert: presentation entity equals: nil.
	presentation initializePresentation.
	self assert: (pane port: #entity) value equals: 2.
	self assert: presentation entity equals: 2
]

{ #category : #tests }
GLMPresentationTest >> testEquality [
	| presentation |
	presentation := GLMPresentation new.
	self assert: (presentation = presentation copy) not
]

{ #category : #tests }
GLMPresentationTest >> testExplicitNotNilMultipleValues [
	| presentation | 
	presentation := GLMPresentation new. 
	presentation useExplicitNotNil.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil nil)).
	self assert: presentation matches not.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(1 nil)).
	self assert: presentation matches.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil 1)).
	self assert: presentation matches.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(1 2)).
	self assert: presentation matches.
]

{ #category : #tests }
GLMPresentationTest >> testHasTitle [
	| presentation |
	presentation := GLMPresentation new.
	self assert: presentation hasTitle not.
	presentation title: 'something'.
	self assert: presentation hasTitle
]

{ #category : #tests }
GLMPresentationTest >> testHasTitleIcon [
	| presentation |
	presentation := GLMPresentation new.
	self assert: presentation hasTitleIcon not.
	presentation titleIcon: 'something'.
	self assert: presentation hasTitleIcon
]

{ #category : #tests }
GLMPresentationTest >> testImplicitNotNilMultipleValues [
	| presentation | 
	presentation := GLMPresentation new. 
	presentation useImplicitNotNil.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil nil)).
	self assert: presentation matches not.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(1 nil)).
	self assert: presentation matches not.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil 1)).
	self assert: presentation matches not.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(1 2)).
	self assert: presentation matches.
]

{ #category : #tests }
GLMPresentationTest >> testMatching [
	| presentation |
	presentation := GLMPresentation new.
	self assert: presentation usesImplicitNotNil.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil 1)).
	self assert: presentation matches not.
	(presentation pane port: #entity) value: 1.
	self assert: presentation matches.
	presentation useExplicitNotNil.
	self assert: presentation usesImplicitNotNil not.
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil 1)).
	self assert: presentation matches.
	(presentation pane port: #entity) value: 1.
	self assert: presentation matches.
	presentation condition: [ :x | x > 1 ].
	presentation useImplicitNotNil.
	self assert: presentation usesImplicitNotNil.
	(presentation pane port: #entity) value: nil.
	self assert: presentation matches not.
	(presentation pane port: #entity) value: 1.
	self assert: presentation matches not.
	(presentation pane port: #entity) value: 2.
	self assert: presentation matches.
	presentation useExplicitNotNil.
	(presentation pane port: #entity) value: 1.
	self assert: presentation matches not.
	(presentation pane port: #entity) value: 2.
	self assert: presentation matches.
	self assert: presentation usesImplicitNotNil not.
	[ 
	(presentation pane port: #entity) value: (GLMMultiValue withAll: #(nil 1)).
	presentation matches.
	self signalFailure: 'Should have raised error' ]
		on: MessageNotUnderstood
		do: [ :err | 
			self assert: err message selector equals: #>.
			self assert: err message arguments equals: (Array with: 1) ]
]

{ #category : #tests }
GLMPresentationTest >> testOnChangeOfPortAct [
	| pane presentation timesTriggered |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	timesTriggered := 0.
	presentation 
		onChangeOfPort: #test 
		act: [ timesTriggered := timesTriggered + 1 ].
	(pane port: #test) value: 42.	
	self assert: timesTriggered equals: 1
]

{ #category : #tests }
GLMPresentationTest >> testPresentationInitializationBlock [
	| pane presentation count |
	count := 0.
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new initialize: [ :pres | (pres pane port: #test) value: count ].
	pane addPresentation: presentation.
	self assert: (pane port: #test) value equals: nil.
	presentation initializePresentation.
	self assert: (pane port: #test) value equals: 0.
	self assert: (presentation pane port: #test) value equals: 0.
	count := 1.
	presentation initializePresentation.
	self assert: (pane port: #test) value equals: 1.
	self assert: (presentation pane port: #test) value equals: 1
]

{ #category : #tests }
GLMPresentationTest >> testPresentationInitializationBlockIsCalled [
	| pane presentation count |
	count := 0.
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new initialize: [ :pres | count := count + 1 ].
	pane addPresentation: presentation.
	self assert: count equals: 0.
	presentation initializePresentation.
	self assert: count equals: 1.
	presentation initializePresentation.
	self assert: count equals: 2
]

{ #category : #tests }
GLMPresentationTest >> testRawSelectionAvailableWhenNoSelectionActive [
"testSelectionNotAvailableWhenNoSelectionActive
testSelectionAvailableWhenNoSelectionDisabled"
	| presentation |
	presentation := GLMPresentation new.
	presentation acceptsSelection: false.

	presentation selection: 1.
	self assert: presentation selection equals: nil.
	self assert: presentation rawSelection equals: 1
]

{ #category : #tests }
GLMPresentationTest >> testSelectionActions [
	| listing |
	listing := GLMPresentation new.
	listing selectionActions isEmpty.
	listing addSelectionAction: GLMAction new.
	self assert: listing selectionActions size equals: 1.
]

{ #category : #tests }
GLMPresentationTest >> testSelectionAvailableWhenNoSelectionDisabled [
	| presentation |
	presentation := GLMPresentation new.

	presentation selection: 1.
	self assert: presentation selection equals: 1.
	self assert: presentation rawSelection equals: 1
]

{ #category : #tests }
GLMPresentationTest >> testSetup [
	| presentation |
	presentation := GLMPresentation new. 
	self assert: presentation pane notNil. 
	self assert: (presentation pane presentations includes: presentation)
]

{ #category : #tests }
GLMPresentationTest >> testTitle [
	| presentation |
	presentation := GLMPresentation new.
	presentation entity: #foo.
	self assert: presentation titleValue isNil.
	presentation title: '41'.
	self assert: presentation titleValue equals: '41'.
	presentation entity: '4'.
	presentation title: [ :x | x , '2' ].
	self assert: presentation titleValue equals: '42'.
	presentation entity: (GLMMultiValue withAll: #('4' '3')).
	presentation title: [ :first :second | first , second ].
	self assert: presentation titleValue equals: '43'
]

{ #category : #tests }
GLMPresentationTest >> testToRawSelection [
	| presentation |
	presentation := GLMPresentation new.

	presentation selection: 1.
	self assert: presentation selection equals: 1.
	self assert: presentation rawSelection equals: 1.
	
	presentation selectionTransformation: [ :num  | num + 1].
	presentation selection: 1.
	self assert: presentation selection equals: 2.
	self assert: presentation rawSelection equals: 1.
]

{ #category : #tests }
GLMPresentationTest >> testToSend [
	| presentation |
	presentation := GLMPresentation new.
	presentation selection: #foo.
	self assert: presentation selection equals: #foo.
	presentation selection: #(#foo #bar ).
	self assert: presentation selection equals: #(#foo #bar ).
	presentation selectionTransformation: [ :first :second | (first , second) asSymbol ].
	presentation selection: (GLMMultiValue withAll: #(#foo #bar )).
	self assert: presentation selection equals: #foobar
]

{ #category : #tests }
GLMPresentationTest >> testToSendAs [
	| presentation |
	presentation := GLMPresentation new.
	presentation selection: #foo.
	self assert: presentation selection equals: #foo.
	self assert: (presentation pane port: #newport) value equals: nil.
	presentation send: [ :num | num + 1 ] as: #newport.
	presentation selection: 1.
	self assert: (presentation pane port: #newport) value equals: 2.
	self assert: presentation selection equals: 1
]

{ #category : #tests }
GLMPresentationTest >> testToSendMultipleValue [
	<expectedFailure>
	| presentation |
	presentation := GLMPresentation new.
	presentation selection: (GLMMultiValue withAll: #(#foo #bar )).
	self assert: presentation selection equals: #foo.
	presentation selectionTransformation: [ :first :second | (first , second) asSymbol ].
]

{ #category : #tests }
GLMPresentationTest >> testUnregisterAnnouncements [
	| announcer presentation |
	announcer := Announcer new.
	presentation := GLMPresentation new.
	presentation updateOn: GLMTestAnnouncement from: [ announcer ].
	presentation registerAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 1.
	presentation unregisterFromAllAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 0
]

{ #category : #tests }
GLMPresentationTest >> testUnregisterAnnouncementsWhenEntityChanges [
	| announcer1 presentation |
	announcer1 := Announcer new.
	presentation := GLMPresentation new.
	presentation updateOn: GLMTestAnnouncement from: [ announcer1 ].
	presentation entity: 1.
	presentation registerAnnouncements.
	self assert: announcer1 numberOfSubscriptions equals: 1.
	presentation entity: 2.
	self assert: announcer1 numberOfSubscriptions equals: 0.
	presentation registerAnnouncements.
	self assert: announcer1 numberOfSubscriptions equals: 1.
	presentation entity: nil.
	self assert: announcer1 numberOfSubscriptions equals: 0
]

{ #category : #tests }
GLMPresentationTest >> testUpdate [
	| col pane presentation |
	col := OrderedCollection with: 1 with: 2.
	pane := GLMPane named: 'test'.
	presentation := GLMListPresentation new.
	pane addPresentation: presentation.
	presentation entity: col.
	presentation shouldValidate: true.
	presentation update.
	presentation selection: 2.
	self assert: presentation selection equals: 2.
	col removeLast.
	presentation update.
	self assert: presentation selection isNil
]

{ #category : #tests }
GLMPresentationTest >> testUpdateConditionallyTriggered [
	| pane presentation announcer timesTriggered |
	pane := GLMPane named: 'test'.
	(pane port: #entity) value: 40.
	presentation := GLMPresentation new.
	announcer := Announcer new.
	pane addPresentation: presentation.
	timesTriggered := 0.
	presentation when: GLMPresentationUpdated do: [ :announcement | timesTriggered := timesTriggered + 1 ].
	presentation updateOn: GLMTestAnnouncement from: [ :x | announcer ] when: [ :ann :entity | ann parameter + entity = 42 ].
	presentation registerAnnouncements.
	announcer announce: (GLMTestAnnouncement new parameter: 0).
	self assert: timesTriggered equals: 0.
	announcer announce: (GLMTestAnnouncement new parameter: 2).
	self assert: timesTriggered equals: 1
]

{ #category : #tests }
GLMPresentationTest >> testUpdateOnFromAllPortsUpdate [
	| pane presentation announcers col updated |
	col := (1 to: 4) asOrderedCollection. 
	pane := GLMPane named: 'test'. 
	presentation := GLMListPresentation new.
	announcers := col collect: [:each | Announcer new].
	pane addPresentation: presentation.
	presentation updateOn: GLMTestAnnouncement fromAll: [ announcers ].
	presentation when: GLMPresentationUpdated do: [:a | updated := true ].
	presentation entity: col.
	presentation shouldValidate: true.
	presentation registerAnnouncements.

	updated := false.
	announcers last announce: GLMTestAnnouncement.
	self assert: updated.
	updated := false.
	announcers last announce: GLMTestAnnouncement.
	self assert: updated.

]

{ #category : #tests }
GLMPresentationTest >> testUpdateOnPortsUpdate [
	| col pane presentation announcer |
	col := OrderedCollection with: 1 with: 2.
	pane := GLMPane named: 'test'.
	presentation := GLMListPresentation new.
	announcer := Announcer new.
	pane addPresentation: presentation.
	presentation updateOn: GLMTestAnnouncement from: [ announcer ].
	presentation entity: col.
	presentation shouldValidate: true.
	presentation registerAnnouncements.
	presentation selection: 2.
	self assert: presentation selection equals: 2.
	col removeLast.
	announcer announce: GLMTestAnnouncement.
	self assert: presentation selection isNil
]

{ #category : #tests }
GLMPresentationTest >> testUpdateOnTriggered [
	| pane presentation announcer timesTriggered |
	pane := GLMPane named: 'test'.
	presentation := GLMPresentation new.
	announcer := Announcer new.
	pane addPresentation: presentation.
	timesTriggered := 0.
	presentation when: GLMPresentationUpdated do: [ :announcement | timesTriggered := timesTriggered + 1 ].
	presentation updateOn: GLMTestAnnouncement from: #yourself.
	presentation entity: announcer.
	presentation registerAnnouncements.
	announcer announce: GLMTestAnnouncement.
	self assert: timesTriggered equals: 1
]

{ #category : #tests }
GLMPresentationTest >> testValidate [
	| pane presentation | 
	pane := GLMPane named: 'test'. 
	presentation := GLMPresentation new.
	pane addPresentation: presentation.
	presentation entity: (OrderedCollection with: 1 with: 2).
	self assert: (presentation validate: 1 on: (pane port: #foo)).
	self assert: (presentation validate: 2 on: (pane port: #foo)).
	self assert: (presentation validate: 5 on: (pane port: #foo))

]

{ #category : #tests }
GLMPresentationTest >> testWith [
	| p presentationInWithBlock |
	p := GLMPresentation new.
	p with: [ :thePresentation | presentationInWithBlock := thePresentation ].
	self assert: presentationInWithBlock identicalTo: p
]

{ #category : #tests }
GLMPresentationTest >> testWithBlockPassesTheParentPresentation [
	| p presentationInWithBlock |
	p := GLMPresentation new.
	p with: [ :thePresentation | presentationInWithBlock := thePresentation ].
	self assert: presentationInWithBlock identicalTo: p
]

{ #category : #tests }
GLMPresentationTest >> testWithReturnsTheOriginalPresentation [
	| p result |
	p := GLMPresentation new.
	result := p with: [ :thePresentation | GLMPresentation new ].
	self assert: result identicalTo: p
]
