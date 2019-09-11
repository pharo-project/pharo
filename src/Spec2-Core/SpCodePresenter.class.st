"
I'm a code (Pharo code) presenter. 
I will style and provide completion according to the behaviour being edited.
"
Class {
	#name : #SpCodePresenter,
	#superclass : #SpTextPresenter,
	#instVars : [
		'#doItContext => SpObservableSlot',
		'#doItReceiver => SpObservableSlot',
		'#behavior => SpObservableSlot',
		'#syntaxHighlight => SpObservableSlot',
		'#contextKeyBindings'
	],
	#category : #'Spec2-Core-Widgets'
}

{ #category : #specs }
SpCodePresenter class >> adapterName [

	^ #CodeAdapter
]

{ #category : #examples }
SpCodePresenter class >> example [
	<sampleInstance>

	^ self new
	 openWithSpec;
		text:
			'some method: 42'

]

{ #category : #specs }
SpCodePresenter class >> title [

	^ 'Code'
]

{ #category : #'api-shout' }
SpCodePresenter >> behavior [
	"Return the class corresponding to the method class of the source code you are editing"
	
	^ behavior
]

{ #category : #'api-shout' }
SpCodePresenter >> behavior: aClass [
	"Set the class corresponding to the method class of the source code you are editing"

	behavior := aClass
]

{ #category : #'api-shortcuts' }
SpCodePresenter >> contextKeyBindings [

	^ contextKeyBindings
]

{ #category : #'api-shortcuts' }
SpCodePresenter >> contextKeyBindings: aDictionary [

	contextKeyBindings := aDictionary
]

{ #category : #'api-doIt' }
SpCodePresenter >> doItContext [

	^ doItContext
]

{ #category : #'api-doIt' }
SpCodePresenter >> doItContext: anObject [

	doItContext := anObject
]

{ #category : #'api-doIt' }
SpCodePresenter >> doItReceiver [
	| receiver |
	
	receiver := doItReceiver.
	receiver isBehavior ifTrue: [ 
		receiver := receiver instanceSide ].
	^ receiver
]

{ #category : #'api-doIt' }
SpCodePresenter >> doItReceiver: anObject [

	doItReceiver := anObject
]

{ #category : #testing }
SpCodePresenter >> hasSyntaxHighlight [

	^ syntaxHighlight
]

{ #category : #initialization }
SpCodePresenter >> initialize [ 

	super initialize.
	
	behavior := nil.
	doItContext := nil.
	doItReceiver := nil.
	self withSyntaxHighlight.
	
	self registerEventsForStyling.
	
	self contextMenu: [ self getMenu ]"self buildCodeMenu".
	"self contextKeyBindings: self buildCodeKeyBindings"
]

{ #category : #initialization }
SpCodePresenter >> registerEventsForStyling [

	self whenBehaviorChangedDo: [ :class | 
		self changed: #classOrMetaClass: with: { class }.
		self changed: #getText ]
]

{ #category : #api }
SpCodePresenter >> selectedBehavior [

	self flag: #TODO. "What this is need for?"
	^ self behavior
]

{ #category : #accessing }
SpCodePresenter >> syntaxHighlight: aBoolean [
	syntaxHighlight := aBoolean 
]

{ #category : #'api-events' }
SpCodePresenter >> whenBehaviorChangedDo: aBlock [
	"Set a block to perform when the behavior class changed"
	
	self 
		property: #behavior 
		whenChangedDo: aBlock
]

{ #category : #'api-events' }
SpCodePresenter >> whenSyntaxHighlightChangedDo: aBlock [
	"Set a block to perform when the syntax highlight is enabled/disabled"

	self property: #syntaxHighlight whenChangedDo: aBlock
]

{ #category : #api }
SpCodePresenter >> withSyntaxHighlight [
	self syntaxHighlight: true
]

{ #category : #api }
SpCodePresenter >> withoutSyntaxHighlight [
	self syntaxHighlight: false
]
