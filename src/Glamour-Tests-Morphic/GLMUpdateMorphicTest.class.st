Class {
	#name : #GLMUpdateMorphicTest,
	#superclass : #GLMMorphicTest,
	#instVars : [
		'announcer',
		'entity',
		'browser',
		'treeMorph',
		'listMorph',
		'tableMorph',
		'listLabelMorph',
		'unupdatedListMorph',
		'labelMorph'
	],
	#category : #'Glamour-Tests-Morphic'
}

{ #category : #running }
GLMUpdateMorphicTest >> setUp [
	super setUp.
	announcer := Announcer new.
	entity := OrderedCollection with: 1 with: 2 with: 3.
	browser := GLMTabulator new.
	browser column: #one; column: #two.
	browser transmit to: #one; andShow: [ :a |
		a 
"			title: [:x | x printString ];"
			verticallyStackedArrangement.
		a list 
			title: [:x | x printString ];
			updateOn: GLMTestAnnouncement from: [ announcer ];
			shouldValidate: true.
		a tree 
			title: [:x | x printString ];
			children: #();
			updateOn: GLMTestAnnouncement from: [ announcer ];
			shouldValidate: true.
		a table
			title: [:x | x printString ];
			updateOn: GLMTestAnnouncement from: [ announcer ];
			shouldValidate: true.
		a text 
			updateOn: GLMTestAnnouncement from: [ announcer ].
		a list 
			title: 'Not updated automatically';
			shouldValidate: true.
		a label 
			display: [:x | x isEmpty ifTrue: ['#()'] ifFalse: [x last]];
			updateOn: GLMTestAnnouncement from: [ announcer ].			
			 ].
	browser transmit to: #two; from: #one; andShow: [ :a | a text].
	window := browser openOn: entity.
	listLabelMorph := 			window submorphs last submorphs second submorphs first submorphs 
									first tabSelectorMorph submorphs first submorphs first.
	listMorph := 				window submorphs last submorphs second submorphs first submorphs 
									first submorphs first submorphs first submorphs first.
	treeMorph := 				window submorphs last submorphs second submorphs first submorphs 
									second submorphs first submorphs first submorphs first.
	tableMorph := 				window submorphs last submorphs second submorphs first submorphs 
									third submorphs first submorphs first submorphs first.
	unupdatedListMorph := 	window submorphs last submorphs second submorphs first submorphs 
									fifth submorphs first submorphs first submorphs first.
	labelMorph := 				window submorphs last submorphs second submorphs first submorphs 
									last submorphs first submorphs first.
]

{ #category : #tests }
GLMUpdateMorphicTest >> testAdd [
	entity add: 4.
	announcer announce: GLMTestAnnouncement.
	self assert: listMorph roots size equals: 4.
	self assert: listMorph model roots size equals: 4.
	self assert: treeMorph roots size equals: 4.
	self assert: treeMorph model roots size equals: 4.
	self assert: tableMorph roots size equals: 4.
	self assert: tableMorph model roots size equals: 4.
	self assert: labelMorph contents equals: '4'
]

{ #category : #tests }
GLMUpdateMorphicTest >> testAddAndUpdateBrowserManually [
	entity add: 4.
	self assert: unupdatedListMorph model roots size equals: 3.
	browser update.
	self assert: unupdatedListMorph model roots size equals: 4.
	entity add: 5.
	browser panes first presentations update.
	self assert: unupdatedListMorph model roots size equals: 5
]

{ #category : #tests }
GLMUpdateMorphicTest >> testAnnouncerUnregistration [
	self assert: announcer numberOfSubscriptions equals: 5.
	browser entity: #($a $b).
	self assert: announcer numberOfSubscriptions equals: 5.
	browser entity: #($d).
	self assert: announcer numberOfSubscriptions equals: 5.
	browser entity: #().
	self assert: announcer numberOfSubscriptions equals: 5.
	browser unregisterFromAllAnnouncements.
	self assert: announcer numberOfSubscriptions equals: 0
]

{ #category : #tests }
GLMUpdateMorphicTest >> testInvalidateSelection [
	(browser panes first port: #selection) value: 3.
	entity removeLast.
	announcer announce: GLMTestAnnouncement.
	self assert: (browser panes first port: #selection) value isNil.
	self assert: listMorph model selection isNil
]

{ #category : #tests }
GLMUpdateMorphicTest >> testRemove [
	entity removeLast.
	announcer announce: GLMTestAnnouncement.
	self assert: listMorph model roots size equals: 2.
	self assert: treeMorph model roots size equals: 2.
	self assert: tableMorph model roots size equals: 2.
	self assert: labelMorph contents equals: '2'
]

{ #category : #tests }
GLMUpdateMorphicTest >> testUpdateBrowserTitle [
	browser title: 'something'.
	browser update.
	self assert: window label equals: 'something'
]

{ #category : #tests }
GLMUpdateMorphicTest >> testUpdatePresentationTitle [
	<expectedFailure>
	browser panes first presentations first title: 'something'.
	browser panes first presentations first update.
	self assert: listLabelMorph contents equals: 'something'
]
