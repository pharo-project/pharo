Class {
	#name : 'CoCompletionEngineTest',
	#superclass : 'CompletionEngineTest',
	#category : 'HeuristicCompletion-Tests-Core',
	#package : 'HeuristicCompletion-Tests',
	#tag : 'Core'
}

{ #category : 'testing' }
CoCompletionEngineTest class >> shouldInheritSelectors [

	^ true
]

{ #category : 'testing' }
CoCompletionEngineTest >> hasBindingOf: aSymbol [
	^ false
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testCmdCtrlLeft [

	self setEditorTextWithCaret: 'self asOrder|ed'.

	editor textArea openInWorld.
	interactionEngine openMenu.
	"Force the drawing of the menu, so the detail can get its position..."
	WorldMorph doOneCycle.

	editor keyDown:
		((self keyboardEventFor: Character arrowLeft) buttons:
			 KMModifier meta eventCode).

	self assert: self editorTextWithCaret equals: 'self |asOrdered'.
	self assert: interactionEngine isMenuOpen.

	editor keyDown:
		((self keyboardEventFor: Character arrowLeft) buttons:
			 KMModifier meta eventCode).

	self assert: self editorTextWithCaret equals: '|self asOrdered'.
	self deny: interactionEngine isMenuOpen
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testCmdCtrlRight [

	self setEditorTextWithCaret: 'self asOrder|ed trac'.

	editor textArea openInWorld.
	interactionEngine openMenu.
	"Force the drawing of the menu, so the detail can get its position..."
	WorldMorph doOneCycle.

	editor keyDown:
		((self keyboardEventFor: Character arrowRight) buttons:
			 KMModifier meta eventCode).

	self assert: self editorTextWithCaret equals: 'self asOrdered| trac'.
	self assert: interactionEngine isMenuOpen.

	editor keyDown:
		((self keyboardEventFor: Character arrowRight) buttons:
			 KMModifier meta eventCode).

	self assert: self editorTextWithCaret equals: 'self asOrdered trac|'.
	self deny: interactionEngine isMenuOpen
]

{ #category : 'tests' }
CoCompletionEngineTest >> testCompleteSuper [

	| entries |
	"Unary"
	interactionEngine menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'postCopy ^ super |'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context completionClass: Point.
	entries := interactionEngine context entries.
	self assert: entries first contents equals: 'postCopy'.

	interactionEngine menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'postCopy ^ super po|'.
	self assert: interactionEngine completionToken equals: 'po'.
	interactionEngine context completionClass: Point.
	entries := interactionEngine context entries.
	self assert: entries first contents equals: 'postCopy'.

	interactionEngine menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'postCopy ^ super xx|'.
	self assert: interactionEngine completionToken equals: 'xx'.
	interactionEngine context completionClass: Point.
	entries := interactionEngine context entries.
	self deny: entries first contents equals: 'postCopy'.

	"Binary"
	interactionEngine menuClosed. "clear the controler"
	self setEditorTextWithCaret: '= aaa ^ super |'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context completionClass: Point.
	entries := interactionEngine context entries.
	self assert: entries first contents equals: '= aaa'.

	interactionEngine menuClosed. "clear the controler"
	self setEditorTextWithCaret: '= aaa ^ super xx|'.
	self assert: interactionEngine completionToken equals: 'xx'.
	interactionEngine context completionClass: Point.
	entries := interactionEngine context entries.
	self deny: entries first contents equals: '= aaa'.

	"Keyword"
	interactionEngine menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'at: bbb put: ccc ^ super |'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context completionClass: Point.
	entries := interactionEngine context entries.
	self assert: entries first contents equals: 'at: bbb put: ccc'.

	interactionEngine menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'at: bbb put: ccc ^ super a|'.
	self assert: interactionEngine completionToken equals: 'a'.
	interactionEngine context completionClass: Point.
	entries := interactionEngine context entries.
	self assert: entries first contents equals: 'at: bbb put: ccc'.

	interactionEngine menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'at: bbb put: ccc ^ super xx|'.
	self assert: interactionEngine completionToken equals: 'xx'.
	interactionEngine context completionClass: Point.
	entries := interactionEngine context entries.
	self deny: entries first contents equals: 'at: bbb put: ccc'.

	"Message unknown in superclass, so does not appear in super completion"
	interactionEngine menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'notARedefinition ^ super |'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context completionClass: Point.
	entries := interactionEngine context entries.
	self deny: entries first contents equals: 'notARedefinition'.

	interactionEngine menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'notARedefinition ^ super notARedefinitio|'.
	self assert: interactionEngine completionToken equals: 'notARedefinitio'.
	interactionEngine context completionClass: Point.
	entries := interactionEngine context entries.
	self assertEmpty: entries
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testEndClosesCompletionContext [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size - 5.

	editor textArea openInWorld.
	interactionEngine openMenu.

	editor keystroke: (self keyboardEventFor: Character end).

	self deny: interactionEngine hasCompletionContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testEndGoesToEndOfLine [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size - 5.

	editor textArea openInWorld.
	interactionEngine openMenu.

	editor keyDown: (self keyboardEventFor: Character end).

	self assert: editor caret equals: text size + 1
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testExitingWordClosesCompletionContext [

	self setEditorTextWithCaret: 'self mEthOdThatDoesNotExis|t toto'.

	editor textArea openInWorld.
	interactionEngine openMenu.

	self assert: interactionEngine hasCompletionContext.
	self assert: self editorTextWithCaret equals: 'self mEthOdThatDoesNotExis|t toto'.
	self assert: interactionEngine completionToken equals: 'mEthOdThatDoesNotExis'.
	editor keyDown: (self keyboardEventFor: Character arrowRight).
	self assert: self editorTextWithCaret equals: 'self mEthOdThatDoesNotExist| toto'.
	self assert: interactionEngine completionToken equals: 'mEthOdThatDoesNotExist'.
	self assert: interactionEngine hasCompletionContext.
	editor keyDown: (self keyboardEventFor: Character arrowRight).
	self assert: self editorTextWithCaret equals: 'self mEthOdThatDoesNotExist |toto'.
	self assert: interactionEngine completionToken equals: ''.
	self deny: interactionEngine hasCompletionContext.

	self setEditorTextWithCaret: 'self m|EthOdThatDoesNotExist toto'.
	interactionEngine openMenu.

	self assert: interactionEngine hasCompletionContext.
	self assert: self editorTextWithCaret equals: 'self m|EthOdThatDoesNotExist toto'.
	self assert: interactionEngine completionToken equals: 'm'.
	editor keyDown: (self keyboardEventFor: Character arrowLeft).
	self assert: self editorTextWithCaret equals: 'self |mEthOdThatDoesNotExist toto'.
	self assert: interactionEngine completionToken equals: ''.
	self assert: interactionEngine hasCompletionContext.
	editor keyDown: (self keyboardEventFor: Character arrowLeft).
	self assert: self editorTextWithCaret equals: 'self| mEthOdThatDoesNotExist toto'.
	self assert: interactionEngine completionToken equals: 'self'.
	self deny: interactionEngine hasCompletionContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testHomeClosesCompletionContext [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size - 5.

	editor textArea openInWorld.
	interactionEngine openMenu.

	editor keystroke: (self keyboardEventFor: Character home).

	self deny: interactionEngine hasCompletionContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testHomeGoesToStartOfLine [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size - 5.

	editor textArea openInWorld.
	interactionEngine openMenu.

	editor keyDown: (self keyboardEventFor: Character home).

	self assert: editor caret equals: 1
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testInitialCompletionEngineHasNoContext [

	"If we did no interaction, no completion context should be initialized"

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	self deny: interactionEngine hasCompletionContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testLeftWithoutResultsBroadensSelection [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.

	editor keyDown: (self keyboardEventFor: Character arrowLeft).

	self assert: interactionEngine context completionToken equals: 'mEthOdThatDoesNotExis'
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testLeftWithoutResultsDoesNotCloseContext [

	| text firstContext |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.
	firstContext := interactionEngine context.

	editor keyDown: (self keyboardEventFor: Character arrowLeft).

	self assert: interactionEngine context equals: firstContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testOpenMenuCreatesCompletionContext [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.

	self assert: interactionEngine hasCompletionContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testTypeCharacterWithoutResultsDoesNotCloseContext [

	| text firstContext |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.
	firstContext := interactionEngine context.

	editor keystroke: (self keyboardEventFor: $a).

	self assert: interactionEngine context equals: firstContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testTypeCharacterWithoutResultsNarrowsSelection [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.

	editor keystroke: (self keyboardEventFor: $a).

	self assert: interactionEngine context completionToken equals: 'mEthOdThatDoesNotExist' , 'a'
]
