"
An EntryCompletion is a handler for the driving of the completion menu in a PluggableTextFieldMorph. The completion menu is an IdentifierChooserMorph which is typically built and popup when a character is entered in a PluggableTextFieldMorph. 

Instance Variables
	chooseBlock:		<Block>
	chooser:		<IdentifierChooserMorph>
	dataSourceBlock:		<Block>
	filterBlock:		<Block>
	previousToken:		<String>

chooseBlock
	- One argument block which is evaluated when a token is chosen, the token is passed as argument

chooser
	- The IdentifierChooserMorph which is currently opened

dataSourceBlock
	- The block that is evaluated in order to get the list of items

filterBlock
	- The block used to filter the dataSource list, it takes 2 args, the first is an item from the current dataSource list element, the second is the token fetched from the requestor (the PluggableTextFieldMorph). It returns true if the current dataSource list element is to be kept

previousToken
	- Used to be able to not open the list if the current text in the PluggableTextFieldMorph was the previous chosen one

"
Class {
	#name : #EntryCompletion,
	#superclass : #Object,
	#instVars : [
		'dataSourceBlock',
		'filterBlock',
		'chooseBlock',
		'chooser'
	],
	#category : #'Polymorph-Widgets-Utilities'
}

{ #category : #examples }
EntryCompletion class >> exampleTextEntryWithCompletionDialog [
	"self exampleTextEntryWithCompletionDialog"

	<exampleWidget>
	<sampleInstance>
	| applicants entryCompletion |
	applicants := (Array
		streamContents: [ :strm | 
			Symbol
				allSymbolTablesDo: [ :each | 
					(each notEmpty and: [ each first canBeGlobalVarInitial and: [ Smalltalk globals includesKey: each ] ])
						ifTrue: [ strm nextPut: each ] ] ]) sort.
	entryCompletion := EntryCompletion new
		dataSourceBlock: [ :currText | applicants ];
		filterBlock: [ :currApplicant :currText | currText size > 3 and: [ currApplicant asUppercase includesSubstring: currText asString asUppercase ] ].
	^ UIManager default
		request: 'Enter a class name fragment (at least 4 characters)'
		initialAnswer: ''
		title: 'Text entry with completion example: simple find a class'
		entryCompletion: entryCompletion
]

{ #category : #accessing }
EntryCompletion >> choose: aToken [
	chooseBlock 
		ifNotNil: [self closeChooser.
			chooseBlock value: aToken]
]

{ #category : #accessing }
EntryCompletion >> chooseBlock [

	^ chooseBlock
]

{ #category : #accessing }
EntryCompletion >> chooseBlock: aBlock [

	chooseBlock := aBlock
]

{ #category : #accessing }
EntryCompletion >> chooser [
	^ chooser
]

{ #category : #accessing }
EntryCompletion >> chooserWith: aToken [
	| applicants |
	applicants  := self filteredValuesWith: aToken.
	aToken 
		ifNil: [applicants isEmpty 
			ifFalse: [self setChooserWith: nil labels: applicants]]
		ifNotNil: [ | meaningfulApplicants | 
			meaningfulApplicants := (applicants copyWithout: nil) asSet.
			meaningfulApplicants isEmpty 
				ifFalse: [(meaningfulApplicants size = 1 and: [aToken = meaningfulApplicants anyOne])
					ifFalse: [self setChooserWith: aToken labels: applicants]]].
	^ chooser
]

{ #category : #accessing }
EntryCompletion >> closeChooser [
	chooser 
		ifNotNil: [chooser close.
			chooser := nil].

]

{ #category : #'event-handling' }
EntryCompletion >> closeIfNotNeeded: aMorph [

	(chooser isNotNil and:[ chooser hasKeyboardFocus or: [ aMorph hasKeyboardFocus ] ]) ifFalse: [ self closeChooser ]
]

{ #category : #accessing }
EntryCompletion >> dataSourceBlock [
	^ dataSourceBlock ifNil: [dataSourceBlock := [:token | #()]]
]

{ #category : #accessing }
EntryCompletion >> dataSourceBlock: aBlock [
	dataSourceBlock := aBlock
]

{ #category : #accessing }
EntryCompletion >> filterBlock [

	^ filterBlock ifNil: [filterBlock := [:currApplicant  :currText | true]]
]

{ #category : #accessing }
EntryCompletion >> filterBlock: aBlock [

	filterBlock := aBlock
]

{ #category : #accessing }
EntryCompletion >> filteredValuesWith: aToken [
	^ (self dataSourceBlock value: aToken) 
		select: [:v | aToken isNil or: [aToken isEmpty or: [v isNil or: [self filterBlock value: v value: aToken]]]]
]

{ #category : #'event-handling' }
EntryCompletion >> handlesKeyboard: anEvent [
	^ anEvent keyCharacter = Character arrowDown
		or: [anEvent keyCharacter = Character escape]
]

{ #category : #'event-handling' }
EntryCompletion >> keystroke: anEvent from: aMorph [
	(self handlesKeyboard: anEvent)
		ifFalse: [ ^ false ].
	(anEvent keyCharacter = Character escape)
		ifTrue: [ chooser
				ifNotNil: [ self closeChooser.
					aMorph takeKeyboardFocus ]
				ifNil: [ self openChooserWith: aMorph textMorph text string from: aMorph ].
			^ true ].
	(chooser isNil and: [ anEvent keyCharacter = Character arrowDown ])
		ifTrue: [ self openChooserWith: aMorph textMorph text string from: aMorph ].
	^ chooser ifNil: [ false ] ifNotNil: [ chooser keyStroke: anEvent ]
]

{ #category : #'event-handling' }
EntryCompletion >> keystrokeInChooser: anEvent [
	anEvent keyCharacter = Character escape
		ifTrue: [ chooser := nil ]
]

{ #category : #'event-handling' }
EntryCompletion >> mouseDownFromTextMorph: anEvent [
	self closeChooser
]

{ #category : #accessing }
EntryCompletion >> openChooserWith: aToken from: aFieldMorph [
	self closeChooser.
	self chooserWith: aToken.
	chooser
		ifNotNil: [ | baseColor |
			baseColor := aFieldMorph window ifNil: [ aFieldMorph color veryMuchLighter ] ifNotNil: [ :w | w paneColor veryMuchLighter ].
			chooser baseColor: baseColor.
			chooser oneMenuOfWidth: aFieldMorph bounds width - aFieldMorph layoutInset - (aFieldMorph borderWidth * 2) - 2.
			chooser fillStyle: (aFieldMorph theme textEditorNormalFillStyleFor: aFieldMorph).
			chooser requestor: aFieldMorph.
			chooser position: aFieldMorph bottomLeft + (aFieldMorph borderWidth @ 0).
			chooser boundsInWorld bottomLeft y > chooser allowedArea bottom
				ifTrue: [ chooser forcesHeight: chooser allowedArea bottom - (chooser boundsInWorld topLeft y + 2) ].
			chooser open ].
	^ chooser
]

{ #category : #accessing }
EntryCompletion >> openChooserWithAllOrCloseFrom: aFieldMorph [ 
	chooser 
		ifNotNil: [self closeChooser]
		ifNil: [
			self openChooserWith: aFieldMorph getText asString from: aFieldMorph].
	^ chooser
]

{ #category : #accessing }
EntryCompletion >> positionChooser [
	"Position the chooser to fit on the display."

	
]

{ #category : #accessing }
EntryCompletion >> positionChooser: aChooser [
        "Position the list morph to fit on the display."

        aChooser boundsInWorld bottomLeft y + aChooser listHeight > aChooser world bottom
                ifTrue: [aChooser listMorph
                                        bounds: (aChooser boundsInWorld topLeft - (0 @ aChooser listHeight) extent: aChooser width @ aChooser listHeight)]
                ifFalse: [aChooser listMorph
                                        bounds: (aChooser boundsInWorld bottomLeft extent: aChooser width @ aChooser listHeight)] 
]

{ #category : #accessing }
EntryCompletion >> setChooserWith: aToken labels: labels [
	chooser 
		ifNil: [chooser := IdentifierChooserMorph 
							labels: labels
							chooseBlock: [:token | self choose: token].
					chooser on: #keyStroke send: #keystrokeInChooser: to: self].
	^ chooser
]
