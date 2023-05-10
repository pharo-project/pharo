Class {
	#name : #CoCompletionEngineTest,
	#superclass : #CompletionEngineTest,
	#category : #'HeuristicCompletion-Tests'
}

{ #category : #accessing }
CoCompletionEngineTest >> doItContext [

	^ nil
]

{ #category : #running }
CoCompletionEngineTest >> newCompletionEngine [

	^ CoCompletionEngine new
]

{ #category : #asserting }
CoCompletionEngineTest >> shouldInheritSelectors [

	^ true
]

{ #category : #'tests - interaction' }
CoCompletionEngineTest >> testAdvanceWordDoesNotCloseCompletionContext [
	"Because it will still be at the end of the current word"
	| text |
	text := 'self mEthOdThatDoesNotExist toto'.
	self
		setEditorText: text;
		selectAt: text size - 10.

	editor textArea openInWorld.
	controller openMenu.

	"This is the current shortcut of the completion engine... Cmd+right for all platforms"
	editor keyDown: (self keyboardEventFor: Character arrowRight useCommand: true).

	self assert: controller hasCompletionContext
]

{ #category : #'tests - interaction' }
CoCompletionEngineTest >> testAdvanceWordTwiceClosesCompletionContext [
	"Because it exists current word"
	| text |
	text := 'self mEthOdThatDoesNotExist toto'.
	self
		setEditorText: text;
		selectAt: text size - 10.

	editor textArea openInWorld.
	controller openMenu.

	"This is the current shortcut of the completion engine... Cmd+right for all platforms"
	2 timesRepeat: [
		editor keystroke: (self keyboardEventFor: Character arrowRight useCommand: true) ].

	self deny: controller hasCompletionContext
]

{ #category : #'tests - interaction' }
CoCompletionEngineTest >> testCmdCtrlLeftClosesDetail [

	"The start of a word does not close the completion menu"

	| text |
	text := 'self asOrdered'.
	self
		setEditorText: text;
		selectAt: text size - 1.

	editor textArea openInWorld.
	controller openMenu.

	"Detail already closed? open it"
	controller hasDetail ifFalse: [ controller showDetail ].

	"Force the drawing of the menu, so the detail can get its position..."
	WorldMorph doOneCycle.
	"This is the current shortcut to open the details of the completion engine... Cmd/ctrl+right for all platforms"
	editor keyDown:
		((self keyboardEventFor: Character arrowLeft) buttons:
			 KMModifier meta eventCode).

	self assert: controller hasDetail
]

{ #category : #'tests - interaction' }
CoCompletionEngineTest >> testCmdCtrlRightOpensDetail [

	| text |
	text := 'self asOrdered'.
	self
		setEditorText: text;
		selectAt: text size - 1.

	editor textArea openInWorld.
	controller openMenu.

	"Detail already open? close it"
	controller hasDetail ifTrue: [ controller hideDetail ].

	"Force the drawing of the menu, so the detail can get its position..."
	WorldMorph doOneCycle.
	"This is the current shortcut to open the details of the completion engine... Cmd/ctrl+right for all platforms"
	editor keyDown:
		((self keyboardEventFor: Character arrowRight) buttons:
			 KMModifier meta eventCode).

	self assert: controller hasDetail
]

{ #category : #tests }
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

{ #category : #'tests - interaction' }
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

{ #category : #'tests - interaction' }
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

{ #category : #'tests - interaction' }
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

{ #category : #'tests - interaction' }
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

{ #category : #'tests - interaction' }
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

{ #category : #'tests - interaction' }
CoCompletionEngineTest >> testInitialCompletionEngineHasNoContext [

	"If we did no interaction, no completion context should be initialized"

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	self deny: controller hasCompletionContext
]

{ #category : #'tests - interaction' }
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

{ #category : #'tests - interaction' }
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

{ #category : #'tests - interaction' }
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

{ #category : #'tests - interaction' }
CoCompletionEngineTest >> testPreviousWordClosesCompletionContext [

	| text |
	text := 'self mEthOdThatDoesNotExist toto'.
	self
		setEditorText: text;
		selectAt: text size.

	editor textArea openInWorld.
	controller openMenu.

	"This is the current shortcut of the completion engine... Cmd+left for all platforms"
	editor keystroke: (self keyboardEventFor: Character arrowLeft useCommand: true).

	self deny: controller hasCompletionContext
]

{ #category : #'tests - interaction' }
CoCompletionEngineTest >> testPreviousWordGoesToPreviousWord [

	| text |
	text := 'self mEthOdThatDoesNotExist toto'.
	self
		setEditorText: text;
		selectAt: text size.

	editor textArea openInWorld.
	controller openMenu.

	"This is the current shortcut of the completion engine... Cmd+left for all platforms"
	editor keyDown: (self keyboardEventFor: Character arrowLeft useCommand: true).

	self assert: editor caret equals: text size - 'toto' size + 1
]

{ #category : #'tests - interaction' }
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

{ #category : #'tests - interaction' }
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
