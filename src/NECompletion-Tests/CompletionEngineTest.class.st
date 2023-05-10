"
A NECControllerTest is a test class for testing the behavior of NECController
"
Class {
	#name : #CompletionEngineTest,
	#superclass : #TestCase,
	#instVars : [
		'controller',
		'editor'
	],
	#category : #'NECompletion-Tests'
}

{ #category : #'tests - keyboard' }
CompletionEngineTest >> allSelfClosingCharacters [
	^ {'{}' . '[]' . '()' . '<>' . '||' . '""' . ''''''}
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> allSmartCharacters [
	^ self allSelfClosingCharacters , { '||'. '<>' }
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> editorTextWithCaret [
	"The current test in the editor, with a `|` inserted at the caret position"

	| source |
	source := editor text asString.
	^ (source copyFrom: 1 to: editor caret-1), '|', (source copyFrom: editor caret to: source size)
]

{ #category : #accessing }
CompletionEngineTest >> interactionModel [

	^ nil
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> keyboardEventFor: char [

	^ self keyboardEventFor: char useCommand: false
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> keyboardEventFor: char useCommand: command [

	| event modifier |
	event := KeyboardEvent new.
	modifier := 0.
	command ifTrue: [ modifier := modifier + 64].
	event
		setType: #keyDown
		buttons: modifier
		position:  0@0
		keyValue: char asciiValue
		charCode: char asciiValue
		hand: nil
		stamp: Time now.
	event key: (KeyboardKey fromCharacter: char
		ifNone: [ char = $_
			ifTrue: [ KeyboardKey named: #UNDERSCORE ]
			ifFalse:[ Error signal: 'unsuported' ] ]).
	^event
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> keyboardPressFor: char [

	| event |
	event := KeyboardEvent new.
	event
		setType: #keypress
		buttons: 0
		position:  0@0
		keyValue: char asciiValue
		charCode: char asciiValue
		hand: nil
		stamp: Time now.
	^event
]

{ #category : #running }
CompletionEngineTest >> newCompletionEngine [

	^ CompletionEngine new
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> selectAll [
	editor selectAll
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> selectAt: anIndex [
	editor selectFrom: anIndex to: anIndex - 1
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> selectFrom: start to: stop [
	editor selectFrom: start to: stop
]

{ #category : #accessing }
CompletionEngineTest >> selectedClassOrMetaClass [

	^ nil
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> setEditorText: aString [
	editor selectAll; addString:  aString
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> setEditorTextWithCaret: aString [
	"Helper method. `|` denotes the caret and is removed"

	| index source |
	index := aString indexOf: $|.
	source := (aString copyFrom: 1 to: index-1) , (aString copyFrom: index+1 to: aString size).
	self setEditorText: source.
	self selectAt: index
]

{ #category : #running }
CompletionEngineTest >> setUp [
	super setUp.

	editor := RubSmalltalkEditor forTextArea: RubEditingArea new beForSmalltalkCode.
	controller := self newCompletionEngine.
	controller setEditor: editor.
	editor completionEngine: controller.
	editor textArea model: self
]

{ #category : #running }
CompletionEngineTest >> tearDown [

	controller closeMenu.
	editor textArea delete.
	super tearDown
]

{ #category : #tests }
CompletionEngineTest >> testCompletionAfterKeyword [

	self setEditorTextWithCaret: 'self foo:| baz'.
	self assert: controller completionToken equals: 'foo:'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"foo:|"x"baz'.
	self assert: controller completionToken equals: 'foo:'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.foo:|.baz'.
	self assert: controller completionToken equals: 'foo:'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=foo:|:=baz'.
	self assert: controller completionToken equals: 'foo:'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: 'foo:|'.
	self assert: controller completionToken equals: 'foo:'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : #tests }
CompletionEngineTest >> testCompletionAfterWord [

	self setEditorTextWithCaret: 'self foo| baz'.
	self assert: controller completionToken equals: 'foo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"foo|"x"baz'.
	self assert: controller completionToken equals: 'foo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.foo|.baz'.
	self assert: controller completionToken equals: 'foo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=foo|:=baz'.
	self assert: controller completionToken equals: 'foo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: 'foo|'.
	self assert: controller completionToken equals: 'foo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : #tests }
CompletionEngineTest >> testCompletionBeforeKeywordColumn [

	self setEditorTextWithCaret: 'self foo|: baz'.
	self assert: controller completionToken equals: 'foo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"foo|:"x"baz'.
	self assert: controller completionToken equals: 'foo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.foo|:.baz'.
	self assert: controller completionToken equals: 'foo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=foo|::=baz'.
	self assert: controller completionToken equals: 'foo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: 'foo|:'.
	self assert: controller completionToken equals: 'foo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : #tests }
CompletionEngineTest >> testCompletionBeforeWord [

	self setEditorTextWithCaret: 'self |foo baz'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar|foo baz'.

	self setEditorTextWithCaret: 'self"x"|foo"x"baz'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|foo"x"baz'.

	self setEditorTextWithCaret: 'self.|foo.baz'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|foo.baz'.

	self setEditorTextWithCaret: 'self:=|foo:=baz'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|foo:=baz'.

	self setEditorTextWithCaret: '|foo'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|foo'
]

{ #category : #tests }
CompletionEngineTest >> testCompletionOnBinary [
	"For the moment, binary selectors cannot be completed."

	self setEditorTextWithCaret: 'self +|+ baz'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self +bar|+ baz'
]

{ #category : #tests }
CompletionEngineTest >> testCompletionOnFirstLetter [

	self setEditorTextWithCaret: 'self f|oo baz'.
	self assert: controller completionToken equals: 'f'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"f|oo"x"baz'.
	self assert: controller completionToken equals: 'f'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.f|oo.baz'.
	self assert: controller completionToken equals: 'f'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=f|oo:=baz'.
	self assert: controller completionToken equals: 'f'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: 'f|oo'.
	self assert: controller completionToken equals: 'f'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : #tests }
CompletionEngineTest >> testCompletionOnNoWord [

	self setEditorTextWithCaret: 'self | baz'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"|"x"baz'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.|.baz'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=|:=baz'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: ' | '.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: ' bar| '.

	self setEditorTextWithCaret: '|'.
	self assert: controller completionToken equals: ''.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : #tests }
CompletionEngineTest >> testCompletionOnSingleLetter [

	self setEditorTextWithCaret: 'self f| baz'.
	self assert: controller completionToken equals: 'f'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"f|"x"baz'.
	self assert: controller completionToken equals: 'f'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.f|.baz'.
	self assert: controller completionToken equals: 'f'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=f|:=baz'.
	self assert: controller completionToken equals: 'f'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: 'f|'.
	self assert: controller completionToken equals: 'f'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testCompletionOpenOnUnderscore [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self underscore_'.

	self
		setEditorText: text;
		selectAt: 'self underscore_' size + 1.

	editor textArea openInWorld.
	controller openMenu.
	editor keyDown: (self keyboardEventFor: $_).

	self assert: controller isMenuOpen
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenFollowedByAssignmentWithCaretInTheEndOfWordAfterCaretWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist:= something that follows'.

	"Select just after the colon"
	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist:' size.

	editor textArea openInWorld.
	controller openMenu.

	controller context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist:'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist::= something that follows'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenFollowedByAssignmentWithCaretInTheEndOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist:= something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist' size.

	editor textArea openInWorld.
	controller openMenu.

	controller context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist:= something that follows'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenFollowedByAssignmentWithCaretInTheMiddleOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist:= something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoes' size.

	editor textArea openInWorld.
	controller openMenu.

	controller context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist:= something that follows'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenWithCaretInTheEndOfWordAfterCaretWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist: something that follows'.

	"Select just after the colon"
	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist:' size + 1.

	editor textArea openInWorld.
	controller openMenu.

	self assert: controller completionToken equals: 'mEthOdThatDoesNotExist:'.
	controller context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist:'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist: something that follows'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenWithCaretInTheEndOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist: something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist' size + 1.

	editor textArea openInWorld.
	controller openMenu.

	controller context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist:'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist: something that follows'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenWithCaretInTheMiddleOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist: something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoes' size.

	editor textArea openInWorld.
	controller openMenu.

	controller context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist:'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist: something that follows'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceTokenAfterMovingCaretToMiddleOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist' size + 1.

	editor textArea openInWorld.
	controller openMenu.

	controller context replaceTokenInEditorWith: 'toto'.

	self assert: editor text equals: 'self toto something that follows'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceTokenWithAditionalSpace [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist' size + 1.

	editor textArea openInWorld.
	controller openMenu.

	controller context replaceTokenInEditorWith: 'toto '.

	self assert: editor text equals: 'self toto something that follows'
]

{ #category : #tests }
CompletionEngineTest >> testReplaceTokenWithCaretBeforeEndOfTextWithSpecialCharacterReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := '(self mEthOdThatDoesNotExist)'.
	self
		setEditorText: text;
		selectAt: text size "just before the closing parenthesis".

	editor textArea openInWorld.
	controller openMenu.

	controller context replaceTokenInEditorWith: 'toto'.

	self assert: editor text equals: '(self toto)'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceTokenWithCaretInTheMiddleOfWordReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoes' size + 1.

	editor textArea openInWorld.
	controller openMenu.

	controller context replaceTokenInEditorWith: 'toto'.

	self assert: editor text equals: 'self toto'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceTokenWithCaretInTheMiddleOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoes' size + 1.

	editor textArea openInWorld.
	controller openMenu.
	editor keyDown: (self keyboardEventFor: Character arrowLeft).
	editor keyDown: (self keyboardEventFor: Character arrowLeft).

	controller context replaceTokenInEditorWith: 'toto'.

	self assert: editor text asString equals: 'self toto something that follows'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceTokenWithCaretInTheMiddleOfWordWithFollowingWordsReplacesEntireWord2 [

	| text |
	text := 'self foo something that follows'.

	self
		setEditorText: text;
		selectAt: 'self foo' size + 1.

	editor textArea openInWorld.
	controller openMenu.
	editor keyDown: (self keyboardEventFor: Character arrowLeft).

	controller context replaceTokenInEditorWith: 'aLongString'.

	self assert: editor text asString equals: 'self aLongString something that follows'
]

{ #category : #'tests - interaction' }
CompletionEngineTest >> testReplaceTokenWithCaretOnEndOfWordReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	controller openMenu.

	controller context replaceTokenInEditorWith: 'toto'.

	self assert: editor text equals: 'self toto'
]

{ #category : #tests }
CompletionEngineTest >> testReplaceWithSpaces [

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar '.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar  '.
	self assert: self editorTextWithCaret equals: 'self bar | baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar bar'.
	self assert: self editorTextWithCaret equals: 'self bar bar| baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar bar '.
	self assert: self editorTextWithCaret equals: 'self bar bar| baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar bar  '.
	self assert: self editorTextWithCaret equals: 'self bar bar | baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar  bar'.
	self assert: self editorTextWithCaret equals: 'self bar | bar baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar  bar '.
	self assert: self editorTextWithCaret equals: 'self bar | bar baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar  bar  '.
	self assert: self editorTextWithCaret equals: 'self bar | bar  baz'.
]

{ #category : #tests }
CompletionEngineTest >> testReplaceWithSpaces2 [

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar|.'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar '.
	self assert: self editorTextWithCaret equals: 'self bar |.'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar  '.
	self assert: self editorTextWithCaret equals: 'self bar | .'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar bar'.
	self assert: self editorTextWithCaret equals: 'self bar bar|.'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar bar '.
	self assert: self editorTextWithCaret equals: 'self bar bar |.'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar bar  '.
	self assert: self editorTextWithCaret equals: 'self bar bar | .'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar  bar'.
	self assert: self editorTextWithCaret equals: 'self bar | bar.'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar  bar '.
	self assert: self editorTextWithCaret equals: 'self bar | bar .'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: controller completionToken equals: 'fo'.
	controller context replaceTokenInEditorWith: 'bar  bar  '.
	self assert: self editorTextWithCaret equals: 'self bar | bar  .'.
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartBackspace [
	"Pressing backspace inside paired smart characters should remove both of them"
	self allSmartCharacters
		do: [ :chars |
			self
				setEditorText: '.' , chars , ';';
				selectAt: 3.
			self
				assert: controller smartBackspace
				description: 'smartbackspace if next to smart characters [' , chars , ']'.
			self assert: editor text equals: '.;' ]
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartBackspaceOutsideSmartCharacters [

	self setEditorText: '.[];'; selectAll.
	self selectAt: 2.
	self
		assert: controller smartBackspace not
		description: 'smartbackspace ignored if before smart character'.

	self selectAt: 4.
	self
		assert: controller smartBackspace not
		description: 'smartbackspace ignored if after smart character'
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartBackspaceWithSelection [

	self setEditorText: 'Hello'; selectAll.
	self assert: controller smartBackspace not description: 'No smartbackspace if selection'.

	self selectAt: 2.
	self
		assert: controller smartBackspace not
		description: 'No smartbackspace if no selection and not a smart character'
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterAfterEspace [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: 'a b';
		selectFrom: 2 to: 4.
	editor keystroke: (self keyboardPressFor: $").
	self assert: editor text equals: 'a" b"'
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterPairForIfAbsent [

	"Check leading smart character has matching trailing character - e.g. { and }"
	self allSmartCharacters do: [ :pairs |
		self assert: (controller smartCharacterPairFor: pairs first ifAbsent: [ nil ]) equals: pairs first -> pairs last].

	"Check trailing smart character has matching leading character - e.g. } and {"
	self allSmartCharacters do: [ :pairs |
		self assert: (controller smartCharacterPairFor: pairs last ifAbsent: [ nil ]) equals: pairs first -> pairs last].

	self assert: (controller smartCharacterPairFor: $% ifAbsent: [ nil ]) equals: nil
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterWithEventAndSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.

	controller smartCharacterWithEvent: (self keyboardEventFor: $|).
	self assert: editor text equals: ' |text| '.

	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.

	controller smartCharacterWithEvent: (self keyboardEventFor: $[).
	self assert: editor text equals: ' [text] '.

	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.
	controller smartCharacterWithEvent: (self keyboardEventFor: $]).
	self assert: editor text equals: ' [text] '
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterWithEventSelfClosing [
	self
		setEditorText: ' ';
		selectAt: 2.

	controller smartCharacterWithEvent: (self keyboardEventFor: $().
	self assert: editor text equals: ' ()'.

	self
		setEditorText: ' ';
		selectAt: 2.

	controller smartCharacterWithEvent: (self keyboardEventFor: $)).
	self assert: editor text equals: ' '.

	self
		setEditorText: ' ';
		selectAt: 2.

	controller smartCharacterWithEvent: (self keyboardEventFor: $<).
	self assert: editor text equals: ' '
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartCharactersOppositeOf [

	self allSmartCharacters
		do: [ :pairs |
			self
				assert: (controller smartCharacterOppositeOf: pairs first ifAbsent: [ nil ])
				equals: pairs last ]
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartCharactersShouldClose [
	"Some smart characters shouldn't auto close, ohers not"
	self assert: (controller smartCharacterShouldClose: $() equals: true.
	self assert: (controller smartCharacterShouldClose: $<) equals: false
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartDoubleQuoteDoesNotEscapeQuote [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' te''xt ';
		selectFrom: 2 to: 6.

	controller smartCharacterWithEvent: (self keyboardPressFor: $").
	self assert: editor text equals: ' "te''xt" '
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartDoubleQuoteSurroundsEscapesSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' te"xt ';
		selectFrom: 2 to: 6.

	controller smartCharacterWithEvent: (self keyboardPressFor: $").
	self assert: editor text equals: ' "te""xt" '
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartDoubleQuoteSurroundsSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.

	controller smartCharacterWithEvent: (self keyboardPressFor: $").
	self assert: editor text equals: ' "text" '
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartNeedExtraRemoveInForOppositeAt [

	self assert: (controller smartNeedExtraRemoveIn: '()' for: $( opposite:$) at:1).
	self deny: (controller smartNeedExtraRemoveIn: '(()' for: $( opposite:$) at:2).
	self assert: (controller smartNeedExtraRemoveIn: '(1()' for: $( opposite:$) at:3).
	self assert: (controller smartNeedExtraRemoveIn: '(1(1())' for: $( opposite:$) at:5).
	self assert: (controller smartNeedExtraRemoveIn: '((1)1())))' for: $( opposite:$) at:6).
	self deny: (controller smartNeedExtraRemoveIn: '()(()' for: $( opposite:$) at:3).
	self assert: (controller smartNeedExtraRemoveIn: '(foobar()' for: $( opposite:$) at:8)
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartQuoteSurroundsDoesNotEscapeDoubleQuote [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' te"xt ';
		selectFrom: 2 to: 6.

	controller smartCharacterWithEvent: (self keyboardPressFor: $').
	self assert: editor text equals: ' ''te"xt'' '
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartQuoteSurroundsEscapesSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' te''xt ';
		selectFrom: 2 to: 6.

	controller smartCharacterWithEvent: (self keyboardPressFor: $').
	self assert: editor text equals: ' ''te''''xt'' '
]

{ #category : #'tests - keyboard' }
CompletionEngineTest >> testSmartQuoteSurroundsSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.

	controller smartCharacterWithEvent: (self keyboardPressFor: $').
	self assert: editor text equals: ' ''text'' '
]
