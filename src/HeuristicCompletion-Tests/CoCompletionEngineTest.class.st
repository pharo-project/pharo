Class {
	#name : 'CoCompletionEngineTest',
	#superclass : 'CompletionEngineTest',
	#instVars : [
		'oldSort'
	],
	#category : 'HeuristicCompletion-Tests',
	#package : 'HeuristicCompletion-Tests'
}

{ #category : 'testing' }
CoCompletionEngineTest class >> shouldInheritSelectors [

	^ true
]

{ #category : 'accessing' }
CoCompletionEngineTest >> doItContext [

	^ nil
]

{ #category : 'testing' }
CoCompletionEngineTest >> hasBindingOf: aSymbol [
	^ false
]

{ #category : 'running' }
CoCompletionEngineTest >> newCompletionEngine [

	^ CoCompletionEngine new
]

{ #category : 'running' }
CoCompletionEngineTest >> setUp [
	super setUp.
	oldSort := CompletionContext sorterClass.
	CompletionContext sorterClass: NoSorter.
]

{ #category : 'running' }
CoCompletionEngineTest >> tearDown [
	CompletionContext sorterClass: oldSort.
	super tearDown.	
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testCmdCtrlLeft [

	self setEditorTextWithCaret: 'self asOrder|ed'.

	editor textArea openInWorld.
	controller openMenu.
	"Force the drawing of the menu, so the detail can get its position..."
	WorldMorph doOneCycle.

	editor keyDown:
		((self keyboardEventFor: Character arrowLeft) buttons:
			 KMModifier meta eventCode).

	self assert: self editorTextWithCaret equals: 'self |asOrdered'.
	self assert: controller isMenuOpen.

	editor keyDown:
		((self keyboardEventFor: Character arrowLeft) buttons:
			 KMModifier meta eventCode).

	self assert: self editorTextWithCaret equals: '|self asOrdered'.
	self deny: controller isMenuOpen
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testCmdCtrlRight [

	self setEditorTextWithCaret: 'self asOrder|ed trac'.

	editor textArea openInWorld.
	controller openMenu.
	"Force the drawing of the menu, so the detail can get its position..."
	WorldMorph doOneCycle.

	editor keyDown:
		((self keyboardEventFor: Character arrowRight) buttons:
			 KMModifier meta eventCode).

	self assert: self editorTextWithCaret equals: 'self asOrdered| trac'.
	self assert: controller isMenuOpen.

	editor keyDown:
		((self keyboardEventFor: Character arrowRight) buttons:
			 KMModifier meta eventCode).

	self assert: self editorTextWithCaret equals: 'self asOrdered trac|'.
	self deny: controller isMenuOpen
]

{ #category : 'tests' }
CoCompletionEngineTest >> testCompleteSuper [

	| entries |
	"Unary"
	controller menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'postCopy ^ super |'.
	self assert: controller completionToken equals: ''.
	controller context completionClass: Point.
	entries := controller context entries.
	self assert: entries first contents equals: 'postCopy'.

	controller menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'postCopy ^ super po|'.
	self assert: controller completionToken equals: 'po'.
	controller context completionClass: Point.
	entries := controller context entries.
	self assert: entries first contents equals: 'postCopy'.

	controller menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'postCopy ^ super xx|'.
	self assert: controller completionToken equals: 'xx'.
	controller context completionClass: Point.
	entries := controller context entries.
	self deny: entries first contents equals: 'postCopy'.

	"Binary"
	controller menuClosed. "clear the controler"
	self setEditorTextWithCaret: '= aaa ^ super |'.
	self assert: controller completionToken equals: ''.
	controller context completionClass: Point.
	entries := controller context entries.
	self assert: entries first contents equals: '= aaa'.

	controller menuClosed. "clear the controler"
	self setEditorTextWithCaret: '= aaa ^ super xx|'.
	self assert: controller completionToken equals: 'xx'.
	controller context completionClass: Point.
	entries := controller context entries.
	self deny: entries first contents equals: '= aaa'.

	"Keyword"
	controller menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'at: bbb put: ccc ^ super |'.
	self assert: controller completionToken equals: ''.
	controller context completionClass: Point.
	entries := controller context entries.
	self assert: entries first contents equals: 'at: bbb put: ccc'.

	controller menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'at: bbb put: ccc ^ super a|'.
	self assert: controller completionToken equals: 'a'.
	controller context completionClass: Point.
	entries := controller context entries.
	self assert: entries first contents equals: 'at: bbb put: ccc'.

	controller menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'at: bbb put: ccc ^ super xx|'.
	self assert: controller completionToken equals: 'xx'.
	controller context completionClass: Point.
	entries := controller context entries.
	self deny: entries first contents equals: 'at: bbb put: ccc'.

	"Message unknown in superclass, so does not appear in super completion"
	controller menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'notARedefinition ^ super |'.
	self assert: controller completionToken equals: ''.
	controller context completionClass: Point.
	entries := controller context entries.
	self deny: entries first contents equals: 'notARedefinition'.

	controller menuClosed. "clear the controler"
	self setEditorTextWithCaret: 'notARedefinition ^ super notARedefinitio|'.
	self assert: controller completionToken equals: 'notARedefinitio'.
	controller context completionClass: Point.
	entries := controller context entries.
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
	controller openMenu.

	editor keystroke: (self keyboardEventFor: Character end).

	self deny: controller hasCompletionContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testEndGoesToEndOfLine [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size - 5.

	editor textArea openInWorld.
	controller openMenu.

	editor keyDown: (self keyboardEventFor: Character end).

	self assert: editor caret equals: text size + 1
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testExitingWordClosesCompletionContext [

	self setEditorTextWithCaret: 'self mEthOdThatDoesNotExis|t toto'.

	editor textArea openInWorld.
	controller openMenu.

	self assert: controller hasCompletionContext.
	self assert: self editorTextWithCaret equals: 'self mEthOdThatDoesNotExis|t toto'.
	self assert: controller completionToken equals: 'mEthOdThatDoesNotExis'.
	editor keyDown: (self keyboardEventFor: Character arrowRight).
	self assert: self editorTextWithCaret equals: 'self mEthOdThatDoesNotExist| toto'.
	self assert: controller completionToken equals: 'mEthOdThatDoesNotExist'.
	self assert: controller hasCompletionContext.
	editor keyDown: (self keyboardEventFor: Character arrowRight).
	self assert: self editorTextWithCaret equals: 'self mEthOdThatDoesNotExist |toto'.
	self assert: controller completionToken equals: ''.
	self deny: controller hasCompletionContext.

	self setEditorTextWithCaret: 'self m|EthOdThatDoesNotExist toto'.
	controller openMenu.

	self assert: controller hasCompletionContext.
	self assert: self editorTextWithCaret equals: 'self m|EthOdThatDoesNotExist toto'.
	self assert: controller completionToken equals: 'm'.
	editor keyDown: (self keyboardEventFor: Character arrowLeft).
	self assert: self editorTextWithCaret equals: 'self |mEthOdThatDoesNotExist toto'.
	self assert: controller completionToken equals: ''.
	self assert: controller hasCompletionContext.
	editor keyDown: (self keyboardEventFor: Character arrowLeft).
	self assert: self editorTextWithCaret equals: 'self| mEthOdThatDoesNotExist toto'.
	self assert: controller completionToken equals: 'self'.
	self deny: controller hasCompletionContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testHomeClosesCompletionContext [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size - 5.

	editor textArea openInWorld.
	controller openMenu.

	editor keystroke: (self keyboardEventFor: Character home).

	self deny: controller hasCompletionContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testHomeGoesToStartOfLine [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size - 5.

	editor textArea openInWorld.
	controller openMenu.

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

	self deny: controller hasCompletionContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testLeftWithoutResultsBroadensSelection [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	controller openMenu.

	editor keyDown: (self keyboardEventFor: Character arrowLeft).

	self assert: controller context completionToken equals: 'mEthOdThatDoesNotExis'
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testLeftWithoutResultsDoesNotCloseContext [

	| text firstContext |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	controller openMenu.
	firstContext := controller context.

	editor keyDown: (self keyboardEventFor: Character arrowLeft).

	self assert: controller context equals: firstContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testOpenMenuCreatesCompletionContext [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	controller openMenu.

	self assert: controller hasCompletionContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testTypeCharacterWithoutResultsDoesNotCloseContext [

	| text firstContext |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	controller openMenu.
	firstContext := controller context.

	editor keystroke: (self keyboardEventFor: $a).

	self assert: controller context equals: firstContext
]

{ #category : 'tests - interaction' }
CoCompletionEngineTest >> testTypeCharacterWithoutResultsNarrowsSelection [

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	controller openMenu.

	editor keystroke: (self keyboardEventFor: $a).

	self assert: controller context completionToken equals: 'mEthOdThatDoesNotExist' , 'a'
]
