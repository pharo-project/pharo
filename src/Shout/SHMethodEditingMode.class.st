"
This example shows how to use shout for method editing. Try it with:

SHMethodEditingMode new open

For method editing, you need to provide a class to the styler. When a class is given, the styler is automatically turned into a method editing mode.
The styler is then able to style the code according to the class bindings (instance variables names, shared pool...)
See #shoutAboutToStyle. 
  - First, the class is given to the styler by sending #classOrMetaClass: to the view with the class as argument.
  - Finally, in this example, #shoutAboutToStyle returns true because the code is always to be styled.

Instance Variables
	className:		<String>
	code:		<String>

className
	- The class name for the styler

code
	- the code which is entered into the editor  

"
Class {
	#name : #SHMethodEditingMode,
	#superclass : #Model,
	#instVars : [
		'code',
		'className',
		'selector',
		'classIsMeta'
	],
	#category : #Shout-Examples
}

{ #category : #accessing }
SHMethodEditingMode >> classIsMeta [
	^ classIsMeta ifNil: [classIsMeta := false]
]

{ #category : #accessing }
SHMethodEditingMode >> classIsMeta: aBoolean [
	classIsMeta := aBoolean.
	self selector: nil.
	self changed: #selector.
	self changed: #classIsMeta.
	self code: ''.

]

{ #category : #accessing }
SHMethodEditingMode >> className [
	^ className ifNotNil: [className asSymbol]
]

{ #category : #accessing }
SHMethodEditingMode >> className: aClassName [
	className := aClassName copyWithout: Character cr.
	self selector: nil.
	self changed: #selector.
	self changed: #className.
	self selectedClass
		ifNotNil: [	self changed: #haveClassName]
]

{ #category : #view }
SHMethodEditingMode >> classSelector [
	| applicantsBlock entryCompletion |
	applicantsBlock := [(Array
				streamContents: [:strm | Symbol
						allSymbolTablesDo: [:each | (each notEmpty
									and: [each first canBeGlobalVarInitial
											and: [Smalltalk globals includesKey: each]])
								ifTrue: [strm nextPut: each]]]) sort].
	entryCompletion := EntryCompletion new
				dataSourceBlock: [:currText | applicantsBlock value ];
				filterBlock: [:currApplicant :currText | currText notEmpty
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
SHMethodEditingMode >> classToUse [
	^ self selectedClass
		ifNotNil: [self classIsMeta 
				ifTrue: [self selectedClass class]
				ifFalse: [self selectedClass]]
]

{ #category : #accessing }
SHMethodEditingMode >> code [
	^ code ifNil: [code := '' asText]
]

{ #category : #accessing }
SHMethodEditingMode >> code: someText [
	code := someText.
	self changed: #code
]

{ #category : #accessing }
SHMethodEditingMode >> haveClassName [
	^ self selectedClass notNil
]

{ #category : #view }
SHMethodEditingMode >> metaSwitch [
	^ UIManager default
		newCheckboxFor: self
		getSelected: #classIsMeta
		setSelected: #classIsMeta:
		label: 'Class'
		help: 'Class side is used if checked' 
]

{ #category : #view }
SHMethodEditingMode >> methodSelector [
	| entryCompletion applicantsBlock |
	applicantsBlock := [self selectedClass 
						ifNil: [#()] 
						ifNotNil: [:clsn | self classToUse selectors asArray sort ]].
	entryCompletion := EntryCompletion new
				dataSourceBlock: [:currText | applicantsBlock value];
				filterBlock: [:currApplicant :currText | currText notEmpty
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
SHMethodEditingMode >> open [
	| window editor |
	window := (StandardWindow labelled: 'Method editor with shout') model: self.
	editor := window newTextEditorFor:  self getText:  #code setText: #code: getEnabled: nil. 
	window addMorph: (window newColumn: { window newRow: { self classSelector. self metaSwitch. self methodSelector}. editor} )
		fullFrame: LayoutFrame identity.
	^ window openInWorld
]

{ #category : #accessing }
SHMethodEditingMode >> selectedClass [
	self className ifNil: [ ^ nil ]. 
	^ Smalltalk globals at: self className ifAbsent: []
]

{ #category : #accessing }
SHMethodEditingMode >> selector [
	^ selector
]

{ #category : #accessing }
SHMethodEditingMode >> selector: aSymbol [
	selector := aSymbol ifNotNil: [:sel |( sel copyWithout: Character cr) asSymbol].
	self changed: #selector.
	self classToUse 
		ifNotNil: [:cls | (cls selectors includes: selector)
			ifTrue: [self code: (cls sourceCodeAt: selector)]]
]

{ #category : #shout }
SHMethodEditingMode >> shoutAboutToStyle: aTextView [
	aTextView classOrMetaClass: self classToUse.
	^ true
]
