"
self open
"
Class {
	#name : #RubMethodEditingExample,
	#superclass : #Model,
	#instVars : [
		'textModel',
		'className',
		'selector',
		'classIsMeta'
	],
	#category : #'Rubric-Editing-Widgets-Examples'
}

{ #category : #'instance creation' }
RubMethodEditingExample class >> open [
	self new open
]

{ #category : #accessing }
RubMethodEditingExample >> classIsMeta [
	^ classIsMeta ifNil: [classIsMeta := false]
]

{ #category : #accessing }
RubMethodEditingExample >> classIsMeta: aBoolean [
	classIsMeta := aBoolean.
	self selector: nil.
	self changed: #selector.
	self changed: #classIsMeta.
	self updateCodeWith: ''

]

{ #category : #accessing }
RubMethodEditingExample >> className [
	^ className ifNotNil: [className asSymbol]
]

{ #category : #accessing }
RubMethodEditingExample >> className: aClassName [
	className := aClassName copyWithout: Character cr.
	self selector: nil.
	self updateCodeWith: ''.
	self changed: #selector.
	self changed: #className.
	self selectedClass
		ifNotNil: [ self changed: #haveClassName]
]

{ #category : #view }
RubMethodEditingExample >> classSelector [
	| applicantsBlock entryCompletion |
	applicantsBlock := [(Array
				streamContents: [:strm | Symbol
						allSymbolTablesDo: [:each | (each notEmpty
									and: [each first canBeGlobalVarInitial
											and: [Smalltalk globals includesKey: each]])
								ifTrue: [strm nextPut: each]]]) sort].
	entryCompletion := EntryCompletion new
				dataSourceBlock: [:currText | applicantsBlock value ];
				filterBlock: [:currApplicant :currText | currText size > 0
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].
	^ (UIManager default newAutoAcceptTextEntryFor: self
			get: #className
			set: #className:
			class: String 
			getEnabled: nil
			help: 'Enter a class name' translated
			entryCompletion: entryCompletion)
				acceptOnCR: false;
				ghostText: 'Class name';
				withDropListButton;
				yourself.

]

{ #category : #accessing }
RubMethodEditingExample >> classToUse [
	^ self selectedClass
		ifNotNil: [self classIsMeta 
				ifTrue: [self selectedClass class]
				ifFalse: [self selectedClass]]
]

{ #category : #accessing }
RubMethodEditingExample >> haveClassName [
	^ self selectedClass notNil
]

{ #category : #view }
RubMethodEditingExample >> metaSwitch [
	^ UIManager default
		newCheckboxFor: self
		getSelected: #classIsMeta
		setSelected: #classIsMeta:
		label: 'Class'
		help: 'Class side is used if checked' 
]

{ #category : #view }
RubMethodEditingExample >> methodSelector [
	| entryCompletion applicantsBlock |
	applicantsBlock := [self selectedClass 
						ifNil: [#()] 
						ifNotNil: [:clsn | self classToUse selectors asArray sort ]].
	entryCompletion := EntryCompletion new
				dataSourceBlock: [:currText | applicantsBlock value];
				filterBlock: [:currApplicant :currText | currText size > 0
						and: [currApplicant asUppercase includesSubstring: currText asString asUppercase]].
	^ (UIManager default newAutoAcceptTextEntryFor: self
			get: #selector
			set: #selector:
			class: String 
			getEnabled: #haveClassName
			help: 'Enter a selector' translated
			entryCompletion: entryCompletion) 
				acceptOnCR: false;
				ghostText: 'Selector';
				withDropListButton;
				yourself.

]

{ #category : #view }
RubMethodEditingExample >> newScrolledText [
	| scrolledText |
	scrolledText := self textModel
		newScrolledText
		beForSmalltalkCode;
		withCodeSizeFeedback;
		withGhostText: 'Choose a class and then a method to edit its code';
		yourself.
	scrolledText ghostTextRuler
		centered;
		fontPointSize: 18.
	^ scrolledText
]

{ #category : #updating }
RubMethodEditingExample >> okToChange [
	self textModel canDiscardEdits ifTrue: [ ^ true].
	self promptForCancel ifTrue: [ self textModel clearUserEdits  ].
	^ self textModel canDiscardEdits
]

{ #category : #view }
RubMethodEditingExample >> open [
	| window editor |
	window := (StandardWindow labelled: 'Method editor with shout') model: self.
	editor := self newScrolledText.
	editor hResizing: #spaceFill; vResizing: #spaceFill.
	window addMorph: (window newColumn: { window newRow: { self classSelector. self metaSwitch. self methodSelector}. editor} )
		fullFrame: LayoutFrame identity.
	^ window openInWorld
]

{ #category : #accessing }
RubMethodEditingExample >> promptForCancel [
	"Ask if it is OK to cancel changes"
	
	^(self confirm:
'Changes have not been saved.
Is it OK to cancel changes?' translated)
			
]

{ #category : #accessing }
RubMethodEditingExample >> selectedClass [
	self className ifNil: [ ^ nil ]. 
	^ Smalltalk globals at: self className ifAbsent: []
]

{ #category : #accessing }
RubMethodEditingExample >> selectedClassOrMetaClass [
	^ self classToUse 
]

{ #category : #accessing }
RubMethodEditingExample >> selector [
	^ selector
]

{ #category : #accessing }
RubMethodEditingExample >> selector: aSymbol [
	selector := aSymbol ifNotNil: [:sel |( sel copyWithout: Character cr) asSymbol].
	self updateCodeWith: ''.
	self changed: #selector.
	self classToUse 
		ifNotNil: [:cls | (cls selectors includes: selector)
			ifTrue: [self updateCodeWith:  (cls sourceCodeAt: selector)]]
]

{ #category : #shout }
RubMethodEditingExample >> shoutAboutToStyle: ATextView [

	ATextView classOrMetaClass: self classToUse.
	^ true
]

{ #category : #accessing }
RubMethodEditingExample >> textModel [
	^ textModel ifNil: [ textModel := RubScrolledTextModel new interactionModel: self ]
]

{ #category : #accessing }
RubMethodEditingExample >> updateCodeWith: someText [
	self textModel setText: someText.

]
