"
I'm a code (Pharo code) presenter. 
I will style and provide completion according to the behaviour being edited.
"
Class {
	#name : #CodePresenter,
	#superclass : #TextPresenter,
	#instVars : [
		'#doItContext => SpecObservableSlot',
		'#doItReceiver => SpecObservableSlot',
		'#behavior => SpecObservableSlot',
		'#syntaxHighlight => SpecObservableSlot',
		'#contextKeyBindings'
	],
	#category : #'Spec-Core-Widgets'
}

{ #category : #specs }
CodePresenter class >> adapterName [

	^ #CodeAdapter
]

{ #category : #examples }
CodePresenter class >> example [
	<sampleInstance>

	^ self new
	 openWithSpec;
		text:
			'some method: 42'

]

{ #category : #specs }
CodePresenter class >> title [

	^ 'Code'
]

{ #category : #'api-shout' }
CodePresenter >> behavior [
	"Return the class corresponding to the method class of the source code you are editing"
	
	^ behavior
]

{ #category : #'api-shout' }
CodePresenter >> behavior: aClass [
	"Set the class corresponding to the method class of the source code you are editing"

	behavior := aClass
]

{ #category : #'api-shortcuts' }
CodePresenter >> contextKeyBindings [

	^ contextKeyBindings
]

{ #category : #'api-shortcuts' }
CodePresenter >> contextKeyBindings: aDictionary [

	contextKeyBindings := aDictionary
]

{ #category : #'api-doIt' }
CodePresenter >> doItContext [

	^ doItContext
]

{ #category : #'api-doIt' }
CodePresenter >> doItContext: anObject [

	doItContext := anObject
]

{ #category : #'api-doIt' }
CodePresenter >> doItReceiver [
	| receiver |
	
	receiver := doItReceiver.
	receiver isBehavior ifTrue: [ 
		receiver := receiver instanceSide ].
	^ receiver
]

{ #category : #'api-doIt' }
CodePresenter >> doItReceiver: anObject [

	doItReceiver := anObject
]

{ #category : #testing }
CodePresenter >> hasSyntaxHighlight [

	^ syntaxHighlight
]

{ #category : #initialization }
CodePresenter >> initialize [ 

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
CodePresenter >> registerEventsForStyling [

	self whenBehaviorChangedDo: [ :class | 
		self changed: #classOrMetaClass: with: { class }.
		self changed: #getText ]
]

{ #category : #api }
CodePresenter >> selectedBehavior [

	self flag: #TODO. "What this is need for?"
	^ self behavior
]

{ #category : #accessing }
CodePresenter >> syntaxHighlight: aBoolean [
	syntaxHighlight := aBoolean 
]

{ #category : #'api-events' }
CodePresenter >> whenBehaviorChangedDo: aBlock [
	"Set a block to perform when the behavior class changed"
	
	self 
		property: #behavior 
		whenChangedDo: aBlock
]

{ #category : #'api-events' }
CodePresenter >> whenSyntaxHighlightChangedDo: aBlock [
	"Set a block to perform when the syntax highlight is enabled/disabled"

	self property: #syntaxHighlight whenChangedDo: aBlock
]

{ #category : #api }
CodePresenter >> withSyntaxHighlight [
	self syntaxHighlight: true
]

{ #category : #api }
CodePresenter >> withoutSyntaxHighlight [
	self syntaxHighlight: false
]
