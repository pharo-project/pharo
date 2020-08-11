"
I live as an instance variable in a Browser, Debugger, Workspace or other window. I'm the glue between all participants of the completion system. I create the NECContext and pass myself to the NECMenuMorph. I process the keyboard events and pass them to the NECMenuMorph or close the morph if needed.

The completion process has two starting points, and they are

+ `handleKeystrokeBefore:editor:`
+ `handleKeystrokeAfter:editor:`

These two methods are usually sent from `completionAround:keyStroke:`.

The completion occurs in specific character position. The editor is responsible for determining such positions: look at senders of `atCompletionPosition`
"
Class {
	#name : #CompletionEngine,
	#superclass : #AbstractCompletionEngine,
	#instVars : [
		'contextClass',
		'menuMorph',
		'editor',
		'context',
		'inverseMapping',
		'completionDelay'
	],
	#classVars : [
		'ContextClass'
	],
	#category : #'NECompletion-Morphic'
}

{ #category : #accessing }
CompletionEngine class >> contextClass [
	^ContextClass ifNil: [ CompletionContext ] 
]

{ #category : #accessing }
CompletionEngine class >> contextClass: aClass [
	ContextClass := aClass
]

{ #category : #accessing }
CompletionEngine class >> isCompletionEnabled [
	^NECPreferences enabled
]

{ #category : #testing }
CompletionEngine >> captureNavigationKeys [
	^ NECPreferences captureNavigationKeys
]

{ #category : #'menu morph' }
CompletionEngine >> closeMenu [
	self stopCompletionDelay.
	menuMorph
		ifNotNil: [ menuMorph delete ].
	menuMorph := nil.
]

{ #category : #replacement }
CompletionEngine >> completionToken [

	"Return the word just before the caret"
	^ (self editor text copyFrom: self completionTokenStart to: self editor caret - 1) asString
]

{ #category : #replacement }
CompletionEngine >> completionTokenStart [
	"This is the position in the editor where the completion token starts"
	^ self editor previousWord: self editor caret - 1
]

{ #category : #accessing }
CompletionEngine >> context [
	^context
]

{ #category : #private }
CompletionEngine >> contextClass [
	
	^ contextClass ifNil: [ self class contextClass ]
]

{ #category : #accessing }
CompletionEngine >> contextClass: aClass [ 
	contextClass := aClass
]

{ #category : #'menu morph' }
CompletionEngine >> createContext [
	
	| class |
	class := editor model ifNotNil: [ :model | model selectedClassOrMetaClass ].
	^ self contextClass
		engine: self
		class: class
		source: editor text string
		position: editor caret - 1
]

{ #category : #accessing }
CompletionEngine >> editor [
	^ editor
]

{ #category : #keyboard }
CompletionEngine >> handleKeystrokeAfter: aKeyboardEvent editor: aParagraphEditor [ 
	(aParagraphEditor isNil or: [ self isMenuOpen not ])
		ifTrue: [ ^ self ].
		
	aParagraphEditor atCompletionPosition 
		ifFalse: [ ^ self closeMenu ].
	
	context narrowWith: aParagraphEditor wordAtCaret.
	menuMorph refreshSelection.
]

{ #category : #keyboard }
CompletionEngine >> handleKeystrokeBefore: aKeyboardEvent editor: anEditor [
	"Returns true when I have handled the event and no further processing is needed by the caller,
	and false otherwise.
	
	This message is usually sent by anEditor, requesting completionEngine to handle aKeyboardEvent 
	before evaluating a given block. The given block (in sender) will usually not be evaluated 
	if I return true.
	
	This method would be cleaner if splitted."

	| keyCharacter controlKeyPressed commandKeyPressed |
	self setEditor: anEditor.
	keyCharacter := aKeyboardEvent keyCharacter.
	controlKeyPressed := aKeyboardEvent controlKeyPressed.
	commandKeyPressed := aKeyboardEvent commandKeyPressed.
	
	self isMenuOpen
		ifFalse: [ ^ self handleKeystrokeWithoutMenu: aKeyboardEvent ].
	
	(keyCharacter = Character arrowLeft or: [ keyCharacter = Character arrowRight ])
		ifTrue: [ "just move the caret" ^ false ].
	
	keyCharacter = Character arrowUp
		ifTrue: [ 
			menuMorph moveUp.
			^ true ].
	keyCharacter = Character arrowDown
		ifTrue: [ 
			menuMorph moveDown.
			^ true ].
	keyCharacter = Character pageUp
		ifTrue: [ 
			menuMorph pageUp.
			^ true ].
	keyCharacter = Character pageDown
		ifTrue: [ 
			menuMorph pageDown.
			^ true ].
	(keyCharacter = Character cr and: [ NECPreferences useEnterToAccept ])
		ifTrue: [ 
			menuMorph insertSelected
				ifTrue: [ ^ true ] ].
	keyCharacter = Character tab
		ifTrue: [ 
			menuMorph insertSelected
						ifTrue: [ ^ true ] ].
	keyCharacter = Character backspace
		ifTrue: [ 
			editor isCaretBehindChar
				ifFalse: [ self closeMenu ].
			^ false ].
	(controlKeyPressed not & aKeyboardEvent commandKeyPressed not
		and: [ aKeyboardEvent keyCharacter isCompletionCharacter ])
		ifFalse: [ 
			self closeMenu.
			^ keyCharacter = Character escape ].
	^ false
]

{ #category : #keyboard }
CompletionEngine >> handleKeystrokeWithoutMenu: aKeyboardEvent [
	"I handle resetting the completion menu, and I return true when I handle an event.
	Returning true means that no further processing is needed by the caller."
	
	self editor atCompletionPosition ifFalse: [ 
		^ (self smartInputWithEvent: aKeyboardEvent ) notNil ].	
	
	self stopCompletionDelay.

	(NECPreferences popupShowWithShortcut matches: {aKeyboardEvent})
		 ifTrue: [
			self openMenu.
			^true ].
		
	(self smartInputWithEvent: aKeyboardEvent )
		ifNotNil: [ ^true ].

	NECPreferences popupShowAutomatic ifTrue: [
		(aKeyboardEvent anyModifierKeyPressed not 
		and: [ aKeyboardEvent keyCharacter isCompletionCharacter ]) 
			ifTrue: [ self resetCompletionDelay ] ].
	
	^ false
]

{ #category : #testing }
CompletionEngine >> hasCompletionContext [
	
	^ context notNil
]

{ #category : #keyboard }
CompletionEngine >> invalidateEditorMorph [
	"Redraws just the part of the morph within the editor's bounds"
	editor morph invalidRect: editor morph bounds.

]

{ #category : #'menu morph' }
CompletionEngine >> isMenuOpen [
	^menuMorph notNil
]

{ #category : #testing }
CompletionEngine >> isScripting [

	^ self editor 
		ifNotNil: [ self editor isScripting ]
		ifNil: [ false ]
]

{ #category : #keyboard }
CompletionEngine >> leftArrow [
	"I return false when the arrow is at its left-most position.
	Otherwise i return true."
	
	(menuMorph hideDetail)	
			ifFalse: [ 
				self closeMenu.
				^ false ].	
	^ true
]

{ #category : #'menu morph' }
CompletionEngine >> menuClosed [
	menuMorph := nil.
	context := nil.
]

{ #category : #private }
CompletionEngine >> menuMorphClass [
	^ NECMenuMorph
]

{ #category : #keyboard }
CompletionEngine >> newSmartCharacterInsertionStringForLeft: left right: right [
	((NECPreferences smartCharactersWithDoubleSpace includes: left) or: [
	(NECPreferences smartCharactersWithDoubleSpace includes: right)])
	ifTrue: [ 
		^ String with: left with: Character space with: Character space  with: right ].
	
	((NECPreferences smartCharactersWithSingleSpace includes: left) or: [
	(NECPreferences smartCharactersWithSingleSpace includes: right)])
	ifTrue: [ 
		^ String with: left with: Character space  with: right ].
	
	^ String with: left with: right
]

{ #category : #'menu morph' }
CompletionEngine >> openMenu [
	| theMenu |
	self stopCompletionDelay.

	context := self createContext.
	theMenu := self menuMorphClass
				engine: self
				position: (editor selectionPosition: context completionToken).
				
	theMenu isClosed ifFalse: [ menuMorph := theMenu ]
]

{ #category : #replacement }
CompletionEngine >> replaceTokenInEditorWith: aString [
	"Main API with the completion context.
	Replace the current completion token (as it was extracted by the completion context) by aString.
	aString may contain a keyword selector string where each keyword is separated by two spaces (between:  and:)
	After replacing, set the caret after the first keyword.
	The completion context uses this API to insert text into the text editor"

	| wordEnd old positionAfterFirstKeyword offset firstKeyword wordStart |
	"Try to correctly replace the current token in the word.
	The code editor should be able to do it by himself"
	wordEnd := self editor nextWord: self editor caret.
	wordStart := self editor previousWord: self editor caret - 1.
	self editor
		selectInvisiblyFrom: wordStart
		to: wordEnd - 1.
	old := self editor selection.
	self editor replaceSelectionWith: aString.
	
	offset := NECPreferences spaceAfterCompletion
		ifTrue: [ 1 ]
		ifFalse: [ 0 ].
	firstKeyword := (aString copyUpTo: $ ) size.
	positionAfterFirstKeyword := wordEnd + firstKeyword + offset - old size.
	self editor selectAt: positionAfterFirstKeyword.
	
	self editor morph invalidRect: self editor morph bounds
]

{ #category : #private }
CompletionEngine >> resetCompletionDelay [
	"Open the popup after 100ms and only after certain characters"
	self stopCompletionDelay.
	self isMenuOpen ifTrue: [ ^ self ].
	editor atCompletionPosition ifFalse: [ ^ self ].
	
	completionDelay := [
			(Delay forMilliseconds: NECPreferences popupAutomaticDelay) wait.
			UIManager default defer:  [ 
				editor atCompletionPosition ifTrue: [ self openMenu ]]
		] fork.
	
]

{ #category : #private }
CompletionEngine >> setEditor: anObject [
	editor ifNotNil: [  
		"make sure we unsubscribe from old editor"
		editor morph ifNotNil: [:m | m announcer unsubscribe: self] ].
	editor := anObject.
	editor morph onAnnouncement: MorphLostFocus send: #closeMenu to: self.
	editor morph onAnnouncement: MorphClosePopups send: #closeMenu to: self.
]

{ #category : #keyboard }
CompletionEngine >> smartBackspace [
	| opposite currentText currentEditor smartCharacter |
	
	currentEditor := editor.
	currentEditor hasSelection
		ifTrue: [ ^ false ].
	
	currentText := currentEditor text.
	smartCharacter := currentText at: currentEditor startIndex - 1 ifAbsent: [ ^ false ].	"take the opposite"
	
	opposite := self smartCharacterOppositeOf: smartCharacter ifAbsent: [ ^false ].	"test if the next char is opposite"
	
	opposite = (currentText at: currentEditor stopIndex ifAbsent: [ ^ false ])
		ifFalse: [ ^ false ].	"test if there is an extra opposite to remove"
	
	(self 
		smartNeedExtraRemoveIn: currentText 
		for: smartCharacter 
		opposite: opposite 
		at: currentEditor startIndex)
			ifFalse: [  ^ false ].	
			
	currentEditor closeTypeIn.
	
	currentEditor 
		selectInvisiblyFrom: currentEditor startIndex - 1 
		to: currentEditor stopIndex.
	currentEditor replaceSelectionWith: currentEditor nullText.
	
	self invalidateEditorMorph.
	
	^ true
]

{ #category : #settings }
CompletionEngine >> smartCharacterOppositeOf: char ifAbsent: aBlock [
	^(self smartCharactersMapping at: char ifAbsent: [ ^aBlock value ]) key
]

{ #category : #settings }
CompletionEngine >> smartCharacterPairFor: char ifAbsent: aBlock [
	| left right |
	
	left := self smartCharactersMapping at: char ifPresent: [ char ] ifAbsent: [ 
		self smartInverseMapping at: char ifPresent: [:key | key ] ifAbsent: [ ^aBlock value ]].
	
	right := self smartCharacterOppositeOf: left ifAbsent: [ ^aBlock value ].
	
 	^ left -> right
]

{ #category : #settings }
CompletionEngine >> smartCharacterShouldClose: char [
	"Return true if char is a smart character that should trigger auto-closing, otherwise false"
	^(self smartCharactersMapping at: char ifAbsent: [ ^false ]) value
]

{ #category : #keyboard }
CompletionEngine >> smartCharacterWithEvent: anEvent [
	"char is extracted from anEvent, anEvent is passed because we may need it.
	We may remove char if this is not costly.
	Returns true if anEvent is handled, nil otherwise"

	| opposite previous next char insertion insertionCenter matchingPair |
	char := anEvent keyCharacter.
	editor hasSelection
		ifTrue: [ 
			"Find the matching pair, and if it doesn't exist, we don't handle this event"
			matchingPair := self smartCharacterPairFor: char ifAbsent: [ ^ nil ].
			
			"encloseWith: will handle wrapping or unwrapping the selection"
			editor encloseWith: matchingPair.
			
			"Return true since we've handled this event"
			self invalidateEditorMorph.
			^ true ].
	
	"The editor has no selection in it"
	(self smartCharacterShouldClose: char)	
		ifFalse: [ 
			"char is not a character that should be auto-closed"
			
			"First check if char is a closing character like ) ], etc.
			If it is not a closing character, we're not handling this event, return nil"
			self smartInverseMapping at: char ifAbsent: [ ^ nil ].
			
			"char is a closing character, so blink the previous paren. e.g. if char is ), blink ("
			editor blinkPrevParen: char.
			
			"If there is no next character, we're done handling this event, return nil
			If there is a next character, and it is not the same character as the one that was input (char), 
			we're done handling this event, return nil"
			(editor nextCharacterIfAbsent: [ ^ nil ]) = char
				ifFalse: [ ^ nil ].
			
			"There is a next character, and it is the same as the one that was just input, 
			so advance the cursor, and return true to declare this event handled"
			editor selectAt: editor startIndex + 1.
			self invalidateEditorMorph.
			^ true ].
	
	"char is a smart character that should be auto-closed! Something like (, [, etc.
	Handle the auto-closing by inserting characters in the editor"
	opposite := self smartCharacterOppositeOf: char ifAbsent: [ ^nil ].
	previous := editor previousCharacterIfAbsent: [ Character space ].
	next := editor nextCharacterIfAbsent: [ Character space ].
	insertion := next isSeparator
				ifFalse: [ char asString ]
				ifTrue: [ 
					previous isSeparator
						ifFalse: [ char asString ]
						ifTrue: [ self newSmartCharacterInsertionStringForLeft: char right: opposite ] ].
	editor replaceSelectionWith: insertion.
	insertionCenter := insertion size // 2 max: 1.
	editor selectAt: editor startIndex + insertionCenter.
	self invalidateEditorMorph.
	^ true
]

{ #category : #settings }
CompletionEngine >> smartCharacters [
	^ NECPreferences smartCharacters 
]

{ #category : #settings }
CompletionEngine >> smartCharactersMapping [
	^ NECPreferences smartCharactersMapping 
]

{ #category : #keyboard }
CompletionEngine >> smartInputWithEvent: anEvent [
	"aCharacter is extracted from anEvent, anEvent is passed because we may need it.
	We may remove aCharacter if this is not costly. 
	Returns true if anEvent is handled, nil otherwise"
	
	self smartCharacters 
		ifFalse: [ ^ nil ].
	
	anEvent keyCharacter = Character backspace
		ifTrue: [ self smartBackspace ifTrue: [ ^ true ]].
		
	
	^ self smartCharacterWithEvent: anEvent
]

{ #category : #settings }
CompletionEngine >> smartInverseMapping [
	^ inverseMapping ifNil: [ 
		inverseMapping := Dictionary new.
		self smartCharactersMapping
			keysAndValuesDo: [ :char :inverse | inverseMapping at: inverse key put: char ].
		inverseMapping ]
]

{ #category : #private }
CompletionEngine >> smartNeedExtraRemoveIn: currentText for: opposite [
	"Test if smart remove need to remove an extra character when the smart character 
	 is equal to its opposite"
	
	(currentText select: [ :char | char = opposite ]) size odd
		ifTrue: [ ^ false ].
	^true
]

{ #category : #private }
CompletionEngine >> smartNeedExtraRemoveIn: currentText for: smartCharacter opposite: opposite at: position [
	"Test if we need to remove an extra character when removing a smart character (any kind of smart character)"
	
	smartCharacter = opposite
		ifTrue: [ 
			(self smartNeedExtraRemoveIn: currentText for: opposite)
				ifFalse: [ ^ false ] ]
		ifFalse: [ 
			(self
				smartNeedExtraRemovePairedIn: currentText
				for: smartCharacter
				opposite: opposite
				at: position)
					ifFalse: [ ^false ] ].
	^ true
]

{ #category : #private }
CompletionEngine >> smartNeedExtraRemovePairedIn: currentText for: smartCharacter opposite: opposite at: position [
	"Test if we need to remove an extra character when removed a paired smart character.
	 A paired smart character is any smart character who has an opposite who is diferent to itself: [], ()"
	
	| startIndex countSmart countOpposite |
	
	countSmart := 0.
	countOpposite := 0.
	startIndex := self
		smartStartIndexIn: currentText
		for: smartCharacter
		opposite: opposite
		at: position.
		
	(currentText allButFirst: startIndex) 
		do: [ :char | 
			char = smartCharacter
				ifTrue: [ countSmart := countSmart + 1 ].
			char = opposite
				ifTrue: [ countOpposite := countOpposite + 1 ] ].
	
	(countSmart > countOpposite and: [ (countOpposite - countSmart) odd ])
		ifTrue: [ ^ false ].
		
	^true
]

{ #category : #private }
CompletionEngine >> smartStartIndexIn: currentText for: smartCharacter  opposite: opposite  at: position [
	
	(position - 1) to: 1 by: -1 do: [ :index | | char | 
		char := currentText at: index.
		(char = smartCharacter or: [ char = opposite])
			 ifFalse: [ ^index ] ].
	
	^0
]

{ #category : #private }
CompletionEngine >> stopCompletionDelay [

    completionDelay ifNotNil: [ 
        completionDelay isTerminating ifFalse: [ completionDelay terminate ] ]
]
