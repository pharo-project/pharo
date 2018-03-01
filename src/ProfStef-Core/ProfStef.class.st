"
ProfStef is the Pharo teacher. To start the tutorial, evaluate:

[[[ 
ProfStef go.
]]]
To go to the next lesson evaluate:

[[[
ProfStef next.
]]]

To execute your own tutorial:
[[[
ProfStef goOn: MyOwnTutorial
]]]

MyOwnTutorial should be a subclass of AbstractTutorial.

To see a table of contents with all defined tutorials:

[[[
ProfStef contents 

Metacello Configuration Tutorial Part1
	ProfStef goOn: MetacelloConfigurationTutorialPart1

Metacello Configuration Tutorial Part2
	ProfStef goOn: MetacelloConfigurationTutorialPart2

Metacello Development Cycle
	ProfStef goOn: MetacelloDevelopmentProcess

Inside Metacello Toolbox API
	ProfStef goOn: MetacelloToolBoxTutorial

Pharo Syntax Tutorial
	ProfStef goOn: PharoSyntaxTutorial

'
]]]

Metacello Configuration Tutorial Part1
	ProfStef goOn: MetacelloConfigurationTutorialPart1

Metacello Configuration Tutorial Part2
	ProfStef goOn: MetacelloConfigurationTutorialPart2

Metacello Development Cycle
	ProfStef goOn: MetacelloDevelopmentProcess

Inside Metacello Toolbox API
	PharoTutorial goOn: MetacelloToolBoxTutorial

Pharo Syntax Tutorial
	ProfStef goOn: PharoSyntaxTutorial
'
]]]
"
Class {
	#name : #ProfStef,
	#superclass : #Model,
	#instVars : [
		'lessonView',
		'player'
	],
	#classVars : [
		'Instance'
	],
	#category : #'ProfStef-Core'
}

{ #category : #zen }
ProfStef class >> aboutPharoZen [
	^ 'These are the Pharo Zen values.\\See:\\  ProfStef class>>#pharoZenValues\\Inspect:\\  PharoTutorial pharoZenValues' withCRs
]

{ #category : #cleanup }
ProfStef class >> cleanUp [
	self reset
]

{ #category : #accessing }
ProfStef class >> contents [
	"build table of contents with all defined tutorials"

	^ AbstractTutorial contentsOfTutorials
]

{ #category : #'class initialization' }
ProfStef class >> default [
	^ Instance ifNil: [Instance := self new]
]

{ #category : #navigating }
ProfStef class >> first [
	^ self default first.
]

{ #category : #navigating }
ProfStef class >> go [
	^ self default go.
]

{ #category : #navigating }
ProfStef class >> goOn: aTutorialClass [
	^ self default goOn: aTutorialClass.
]

{ #category : #navigating }
ProfStef class >> goToNextLesson [
  self next
]

{ #category : #navigating }
ProfStef class >> last [
	^ self default last.
]

{ #category : #zen }
ProfStef class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'Pharo Zen')
		parent: #Help;
		order: 3.0;
		action: [ self openPharoZenWorkspace ];
		icon: ((self iconNamed: #pharoIcon) scaledToSize: 16@16);
		help: 'Pharo values.';
		withSeparatorAfter 
]

{ #category : #navigating }
ProfStef class >> next [
	^ self default next.
]

{ #category : #zen }
ProfStef class >> openPharoZenWorkspace [
	"Open a Workspace showing the Pharo Zen values in random order"

	"self openPharoZenWorkspace"

	TextPresenter new
		text: self pharoZenValuesContents;
		title: 'Pharo Zen' translated;
		aboutText: self aboutPharoZen;
		beForText;
		openWithSpec
]

{ #category : #zen }
ProfStef class >> pharoZenValues [
	"Our values and convictions are condensed in this simple list."
	
	^ #(
		'Easy to understand, easy to learn from, easy to change.' 
		'Objects all the way down.' 
		'Examples to learn from.' 
		'Fully dynamic and malleable.' 
		'Beauty in the code, beauty in the comments.' 
		'Simplicity is the ultimate elegance.' 
		'Better a set of small polymorphic classes than a large ugly one.' 
		'Classes structure our vocabulary.' 
		'Messages are our vocabulary.' 
		'Polymorphism is our esperanto.' 
		'Abstraction and composition are our friends.' 
		'Tests are important but can be changed.' 
		'Explicit is better than implicit.' 
		'Magic only at the right place.' 
		'One step at a time.' 
		'There is no unimportant fix.' 
		'Learning from mistakes.' 
		'Perfection can kill movement.' 
		'Quality is an emerging property.' 
		'Simple processes to support progress.' 
		'Communication is key.' 
		'A system with robust abstractions that a single person can understand.' )
]

{ #category : #zen }
ProfStef class >> pharoZenValuesContents [
	"Return a string with CRs listing the Pharo Zen values in random order"
	
	"self pharoZenValuesContents"
	
	^ String streamContents: [ :stream | 
			stream << (self class>>#pharoZenValues) comment withoutQuoting; cr; cr.
			self pharoZenValues shuffled 
				do: [ :each | stream << each ] 
				separatedBy: [ stream cr ] ]
]

{ #category : #navigating }
ProfStef class >> previous [
	^ self default previous.
]

{ #category : #'class initialization' }
ProfStef class >> reset [ 
	Instance := nil
]

{ #category : #starting }
ProfStef class >> tutorial: aTutorialClass lesson: aSelector [
	self default tutorial: aTutorialClass lesson: aSelector.
]

{ #category : #starting }
ProfStef class >> tutorial: aTutorialClass lessonAt: lessonIndex [
	self default tutorial: aTutorialClass lessonAt: lessonIndex.
]

{ #category : #gui }
ProfStef >> close [
	self lessonView close
]

{ #category : #navigating }
ProfStef >> first [
	self player first.
	^ self showCurrentLesson.
]

{ #category : #starting }
ProfStef >> go [
	^ self goOn: PharoSyntaxTutorial.
]

{ #category : #starting }
ProfStef >> goOn: aTutorialClass [
	self player tutorial: aTutorialClass new.
	^ self open.
]

{ #category : #accessing }
ProfStef >> helpBrowserWindow [
	"Yes this is a hack"
	
	^ World systemWindows 
		detect: [ :each | each model class = HelpBrowser ] 
		ifNone: [ nil ]
]

{ #category : #navigating }
ProfStef >> last [
	self player last.
	^ self showCurrentLesson.
]

{ #category : #accessing }
ProfStef >> lessonView [
	^ lessonView ifNil: [lessonView := LessonView new]
]

{ #category : #accessing }
ProfStef >> lessonView: aLessonView [
	lessonView := aLessonView.
]

{ #category : #navigating }
ProfStef >> next [
	self helpBrowserWindow 
		ifNotNil: [ :window | window model next ].
	self lessonView isOpenInWindow 
		ifTrue: [ 
			self player next.
			self showCurrentLesson ]
]

{ #category : #gui }
ProfStef >> open [
	self player first.
	^ self showCurrentLesson.
]

{ #category : #accessing }
ProfStef >> player [ 
	^ player ifNil: [player := TutorialPlayer new]
]

{ #category : #accessing }
ProfStef >> player: aTutorialPlayer [
	player := aTutorialPlayer.
]

{ #category : #navigating }
ProfStef >> previous [
	self helpBrowserWindow 
		ifNotNil: [ :window | window model previous ].
	self lessonView isOpenInWindow 
		ifTrue: [ 
			self player previous.
			self showCurrentLesson ]
]

{ #category : #gui }
ProfStef >> showCurrentLesson [
	| progressInfo lesson |
	lesson := self player currentLesson.
	progressInfo := '(', self tutorialPositionString, '/', self tutorialSizeString, ')'. 
	^ self lessonView 
		showLesson: lesson 
		withTitle: lesson title, ' ', progressInfo.
]

{ #category : #navigating }
ProfStef >> tutorial: aTutorialClass lesson: aSelector [
	| tutorial |
	tutorial := aTutorialClass new.
	self player tutorial: tutorial.
	self tutorial: aTutorialClass lessonAt: (tutorial indexOfLesson: aSelector).
]

{ #category : #navigating }
ProfStef >> tutorial: aTutorialClass lessonAt: lessonIndex [
	self player tutorial: aTutorialClass new.
	self player tutorialPosition: lessonIndex.
	self showCurrentLesson.
]

{ #category : #accessing }
ProfStef >> tutorialPositionString [
	^ player tutorialPosition asString.
]

{ #category : #accessing }
ProfStef >> tutorialSizeString [
	^ self player size asString
]
