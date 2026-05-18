"
A NECControllerTest is a test class for testing the behavior of NECController
"
Class {
	#name : 'CompletionEngineTest',
	#superclass : 'TestCase',
	#instVars : [
		'editor',
		'interactionEngine',
		'toleranceRate'
	],
	#category : 'NECompletion-Tests',
	#package : 'NECompletion-Tests'
}

{ #category : 'tests - keyboard' }
CompletionEngineTest >> allSelfClosingCharacters [
	^ {'{}' . '[]' . '()' . '<>' . '||' . '""' . ''''''}
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> allSmartCharacters [
	^ self allSelfClosingCharacters , { '||'. '<>' }
]

{ #category : 'running' }
CompletionEngineTest >> doItContext [ 
	^ nil
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> editorTextWithCaret [
	"The current test in the editor, with a `|` inserted at the caret position"

	| source |
	source := editor text asString.
	^ (source copyFrom: 1 to: editor caret-1), '|', (source copyFrom: editor caret to: source size)
]

{ #category : 'helpers' }
CompletionEngineTest >> expectText: aString [ 

	self assert: editor text asString equals: aString
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> hasBindingOf: as [

	^ false
]

{ #category : 'accessing' }
CompletionEngineTest >> interactionModel [

	^ nil
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> keyboardEventFor: char [

	^ self keyboardEventFor: char useCommand: false
]

{ #category : 'tests - keyboard' }
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

{ #category : 'tests - keyboard' }
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

{ #category : 'running' }
CompletionEngineTest >> newCompletionEngine [

	^ CoCompletionEngine new
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> selectAll [
	editor selectAll
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> selectAt: anIndex [
	editor selectFrom: anIndex to: anIndex - 1
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> selectFrom: start to: stop [
	editor selectFrom: start to: stop
]

{ #category : 'accessing' }
CompletionEngineTest >> selectedClassOrMetaClass [

	^ nil
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> setEditorText: aString [
	editor selectAll; addString:  aString
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> setEditorTextWithCaret: aString [
	"Helper method. `|` denotes the caret and is removed"

	| index source |
	index := aString indexOf: $|.
	source := (aString copyFrom: 1 to: index-1) , (aString copyFrom: index+1 to: aString size).
	self setEditorText: source.
	self selectAt: index
]

{ #category : 'running' }
CompletionEngineTest >> setUp [
	super setUp.
	
	
	editor := RubSmalltalkEditor forTextArea: RubEditingArea new beForSmalltalkCode.
	interactionEngine := self newCompletionEngine.
	interactionEngine setEditor: editor.
	editor completionEngine: interactionEngine.
	editor textArea model: self.
	
	toleranceRate := CoToleranceFilter rate.
	CoToleranceFilter disable.
]

{ #category : 'running' }
CompletionEngineTest >> tearDown [

	interactionEngine closeMenu.
	editor textArea delete.
	
	CoToleranceFilter rate: toleranceRate.
	super tearDown
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testCompletionOpenOnUnderscore [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self underscore_'.

	self
		setEditorText: text;
		selectAt: 'self underscore_' size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.
	editor keyDown: (self keyboardEventFor: $_).

	self assert: interactionEngine isMenuOpen
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenFollowedByAssignmentWithCaretInTheEndOfWordAfterCaretWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist:= something that follows'.

	"Select just after the colon"
	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist:' size.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist:'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist::= something that follows'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenFollowedByAssignmentWithCaretInTheEndOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist:= something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist' size.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist:= something that follows'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenFollowedByAssignmentWithCaretInTheMiddleOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist:= something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoes' size.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist:= something that follows'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenWithCaretInTheEndOfWordAfterCaretWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist: something that follows'.

	"Select just after the colon"
	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist:' size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.

	self assert: interactionEngine completionToken equals: 'mEthOdThatDoesNotExist:'.
	interactionEngine context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist:'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist: something that follows'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenWithCaretInTheEndOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist: something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist' size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist:'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist: something that follows'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceKeywordTokenWithCaretInTheMiddleOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist: something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoes' size.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist:'.

	self assert: editor text asString equals: 'self mEthOdThatDoesNotExist: something that follows'
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenAfterKeyword [

	self setEditorTextWithCaret: 'self foo:| baz'.
	self assert: interactionEngine completionToken equals: 'foo:'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"foo:|"x"baz'.
	self assert: interactionEngine completionToken equals: 'foo:'.
	self assert: interactionEngine tokenToReplace equals: 'foo:'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.foo:|.baz'.
	self assert: interactionEngine completionToken equals: 'foo:'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=foo:|:=baz'.
	self assert: interactionEngine completionToken equals: 'foo:'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: 'foo:|'.
	self assert: interactionEngine completionToken equals: 'foo:'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceTokenAfterMovingCaretToMiddleOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist' size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'toto'.

	self assert: editor text equals: 'self toto something that follows'
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenAfterWord [

	self setEditorTextWithCaret: 'self foo| baz'.
	self assert: interactionEngine completionToken equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"foo|"x"baz'.
	self assert: interactionEngine completionToken equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.foo|.baz'.
	self assert: interactionEngine completionToken equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=foo|:=baz'.
	self assert: interactionEngine completionToken equals: 'foo'.
	self assert: interactionEngine tokenToReplace equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: 'foo|'.
	self assert: interactionEngine completionToken equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenBeforeKeywordColumn [

	self setEditorTextWithCaret: 'self foo|: baz'.
	self assert: interactionEngine completionToken equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"foo|:"x"baz'.
	self assert: interactionEngine completionToken equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.foo|:.baz'.
	self assert: interactionEngine completionToken equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=foo|::=baz'.
	self assert: interactionEngine completionToken equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: 'foo|:'.
	self assert: interactionEngine completionToken equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenBeforeWord [

	self setEditorTextWithCaret: 'self |foo baz'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar|foo baz'.

	self setEditorTextWithCaret: 'self"x"|foo"x"baz'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|foo"x"baz'.

	self setEditorTextWithCaret: 'self.|foo.baz'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|foo.baz'.

	self setEditorTextWithCaret: 'self:=|foo:=baz'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|foo:=baz'.

	self setEditorTextWithCaret: '|foo'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|foo'
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenOnBinary [
	"For the moment, binary selectors cannot be completed."

	self setEditorTextWithCaret: 'self +|+ baz'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self +bar|+ baz'
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenOnFirstLetter [

	self setEditorTextWithCaret: 'self f|oo baz'.
	self assert: interactionEngine completionToken equals: 'f'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"f|oo"x"baz'.
	self assert: interactionEngine completionToken equals: 'f'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.f|oo.baz'.
	self assert: interactionEngine completionToken equals: 'f'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=f|oo:=baz'.
	self assert: interactionEngine completionToken equals: 'f'.
	self assert: interactionEngine tokenToReplace equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: 'f|oo'.
	self assert: interactionEngine completionToken equals: 'f'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenOnNoWord [

	self setEditorTextWithCaret: 'self | baz'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"|"x"baz'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.|.baz'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=|:=baz'.
	self assert: interactionEngine completionToken equals: ''.
	self assert: interactionEngine tokenToReplace equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: ' | '.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: ' bar| '.

	self setEditorTextWithCaret: '|'.
	self assert: interactionEngine completionToken equals: ''.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenOnSingleLetter [

	self setEditorTextWithCaret: 'self f| baz'.
	self assert: interactionEngine completionToken equals: 'f'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self"x"f|"x"baz'.
	self assert: interactionEngine completionToken equals: 'f'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self"x"bar|"x"baz'.

	self setEditorTextWithCaret: 'self.f|.baz'.
	self assert: interactionEngine completionToken equals: 'f'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self.bar|.baz'.

	self setEditorTextWithCaret: 'self:=f|:=baz'.
	self assert: interactionEngine completionToken equals: 'f'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self:=bar|:=baz'.

	self setEditorTextWithCaret: 'f|'.
	self assert: interactionEngine completionToken equals: 'f'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'bar|'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceTokenWithAdditionalSpace [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoesNotExist' size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'toto '.

	self assert: editor text equals: 'self toto something that follows'
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenWithCaretBeforeEndOfTextWithSpecialCharacterReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := '(self mEthOdThatDoesNotExist)'.
	self
		setEditorText: text;
		selectAt: text size "just before the closing parenthesis".

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'toto'.

	self assert: editor text equals: '(self toto)'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceTokenWithCaretInTheMiddleOfWordReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoes' size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'toto'.

	self assert: editor text equals: 'self toto'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceTokenWithCaretInTheMiddleOfWordWithFollowingWordsReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist something that follows'.

	self
		setEditorText: text;
		selectAt: 'self mEthOdThatDoes' size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.
	editor keyDown: (self keyboardEventFor: Character arrowLeft).
	editor keyDown: (self keyboardEventFor: Character arrowLeft).

	interactionEngine context replaceTokenInEditorWith: 'toto'.

	self assert: editor text asString equals: 'self toto something that follows'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceTokenWithCaretInTheMiddleOfWordWithFollowingWordsReplacesEntireWord2 [

	| text |
	text := 'self foo something that follows'.

	self
		setEditorText: text;
		selectAt: 'self foo' size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.
	editor keyDown: (self keyboardEventFor: Character arrowLeft).

	interactionEngine context replaceTokenInEditorWith: 'aLongString'.

	self assert: editor text asString equals: 'self aLongString something that follows'
]

{ #category : 'tests - interaction' }
CompletionEngineTest >> testReplaceTokenWithCaretOnEndOfWordReplacesEntireWord [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoesNotExist'.
	self
		setEditorText: text;
		selectAt: text size + 1.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'toto'.

	self assert: editor text equals: 'self toto'
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenWithSpaces [

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: interactionEngine completionToken equals: 'fo'.
	self assert: interactionEngine tokenToReplace equals: 'foo'.
	interactionEngine context replaceTokenInEditorWith: 'bar '.
	self assert: self editorTextWithCaret equals: 'self bar| baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  '.
	self assert: self editorTextWithCaret equals: 'self bar | baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar bar'.
	self assert: self editorTextWithCaret equals: 'self bar bar| baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar bar '.
	self assert: self editorTextWithCaret equals: 'self bar bar| baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar bar  '.
	self assert: self editorTextWithCaret equals: 'self bar bar | baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  bar'.
	self assert: self editorTextWithCaret equals: 'self bar | bar baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  bar '.
	self assert: self editorTextWithCaret equals: 'self bar | bar baz'.

	self setEditorTextWithCaret: 'self fo|o baz'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  bar  '.
	self assert: self editorTextWithCaret equals: 'self bar | bar  baz'.
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenWithSpaces2 [

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar|.'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar '.
	self assert: self editorTextWithCaret equals: 'self bar |.'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  '.
	self assert: self editorTextWithCaret equals: 'self bar | .'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar bar'.
	self assert: self editorTextWithCaret equals: 'self bar bar|.'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar bar '.
	self assert: self editorTextWithCaret equals: 'self bar bar |.'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar bar  '.
	self assert: self editorTextWithCaret equals: 'self bar bar | .'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  bar'.
	self assert: self editorTextWithCaret equals: 'self bar | bar.'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  bar '.
	self assert: self editorTextWithCaret equals: 'self bar | bar .'.

	self setEditorTextWithCaret: 'self fo|o.'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  bar  '.
	self assert: self editorTextWithCaret equals: 'self bar | bar  .'.
]

{ #category : 'tests' }
CompletionEngineTest >> testReplaceTokenWithSpaces3 [

	self setEditorTextWithCaret: 'self fo|'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar'.
	self assert: self editorTextWithCaret equals: 'self bar|'.

	self setEditorTextWithCaret: 'self fo|'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar '.
	self assert: self editorTextWithCaret equals: 'self bar |'.

	self setEditorTextWithCaret: 'self fo|'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  '.
	self assert: self editorTextWithCaret equals: 'self bar | '.

	self setEditorTextWithCaret: 'self fo|'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar bar'.
	self assert: self editorTextWithCaret equals: 'self bar bar|'.

	self setEditorTextWithCaret: 'self fo|'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar bar '.
	self assert: self editorTextWithCaret equals: 'self bar bar |'.

	self setEditorTextWithCaret: 'self fo|'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar bar  '.
	self assert: self editorTextWithCaret equals: 'self bar bar | '.

	self setEditorTextWithCaret: 'self fo|'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  bar'.
	self assert: self editorTextWithCaret equals: 'self bar | bar'.

	self setEditorTextWithCaret: 'self fo|'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  bar '.
	self assert: self editorTextWithCaret equals: 'self bar | bar '.

	self setEditorTextWithCaret: 'self fo|'.
	self assert: interactionEngine completionToken equals: 'fo'.
	interactionEngine context replaceTokenInEditorWith: 'bar  bar  '.
	self assert: self editorTextWithCaret equals: 'self bar | bar  '.
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartBackspace [
	"Pressing backspace inside paired smart characters should remove both of them"
	self allSmartCharacters
		do: [ :chars |
			self
				setEditorText: '.' , chars , ';';
				selectAt: 3.
			self
				assert: interactionEngine smartBackspace
				description: 'smartbackspace if next to smart characters [' , chars , ']'.
			self assert: editor text equals: '.;' ]
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartBackspaceOutsideSmartCharacters [

	self setEditorText: '.[];'; selectAll.
	self selectAt: 2.
	self
		assert: interactionEngine smartBackspace not
		description: 'smartbackspace ignored if before smart character'.

	self selectAt: 4.
	self
		assert: interactionEngine smartBackspace not
		description: 'smartbackspace ignored if after smart character'
]

{ #category : 'tests' }
CompletionEngineTest >> testSmartBackspaceRemoveBothCharactersWithSpaces [
	"Test that smart backspace removes both characters even with spaces between them"

	self setEditorText: '(  )'.
	self selectAt: 2.

	self
		assert: interactionEngine smartBackspace
		description: 'Smart backspace should return true'.

	self assert: editor text equals: ''
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartBackspaceWithSelection [

	self setEditorText: 'Hello'; selectAll.
	self assert: interactionEngine smartBackspace not description: 'No smartbackspace if selection'.

	self selectAt: 2.
	self
		assert: interactionEngine smartBackspace not
		description: 'No smartbackspace if no selection and not a smart character'
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterAfterEspace [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: 'a b';
		selectFrom: 2 to: 4.
	editor keystroke: (self keyboardPressFor: $").
	self assert: editor text equals: 'a" b"'
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterPairForIfAbsent [

	"Check leading smart character has matching trailing character - e.g. { and }"
	self allSmartCharacters do: [ :pairs |
		self assert: (interactionEngine smartCharacterPairFor: pairs first ifAbsent: [ nil ]) equals: pairs first -> pairs last].

	"Check trailing smart character has matching leading character - e.g. } and {"
	self allSmartCharacters do: [ :pairs |
		self assert: (interactionEngine smartCharacterPairFor: pairs last ifAbsent: [ nil ]) equals: pairs first -> pairs last].

	self assert: (interactionEngine smartCharacterPairFor: $% ifAbsent: [ nil ]) equals: nil
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterWithEventAndSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.

	interactionEngine smartCharacterWithEvent: (self keyboardEventFor: $|).
	self assert: editor text equals: ' |text| '.

	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.

	interactionEngine smartCharacterWithEvent: (self keyboardEventFor: $[).
	self assert: editor text equals: ' [text] '.

	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.
	interactionEngine smartCharacterWithEvent: (self keyboardEventFor: $]).
	self assert: editor text equals: ' [text] '
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterWithEventDontWritePairAfterDifferentCharacter [
	"Triggering a smart character inside the same smart character pair still works."

	self setEditorTextWithCaret: '$|'.
	editor keystroke: (self keyboardPressFor: $().
	self assert: editor text equals: '$('.

	self setEditorTextWithCaret: '(|'.
	editor keystroke: (self keyboardPressFor: $").
	self assert: editor text equals: '("'.

	self setEditorTextWithCaret: 'foo:|'.
	editor keystroke: (self keyboardPressFor: $[).
	self assert: editor text equals: 'foo:['
]

{ #category : 'tests' }
CompletionEngineTest >> testSmartCharacterWithEventNoPairWhenCharLiteral [

	self setEditorTextWithCaret: '[$|]'.
	editor keystroke: (self keyboardEventFor: $[).
	self assert: editor text equals: '[$[]'.

	self setEditorTextWithCaret: '(1 > $|)'.
	editor keystroke: (self keyboardEventFor: $().
	self assert: editor text equals: '(1 > $()'
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterWithEventSelfClosing [

	self setEditorTextWithCaret: ' |'.
	interactionEngine smartCharacterWithEvent: (self keyboardEventFor: $().
	self assert: editor text equals: ' ()'.

	self setEditorTextWithCaret: ' |'.
	interactionEngine smartCharacterWithEvent: (self keyboardEventFor: $)).
	self assert: editor text equals: ' '.

	self setEditorTextWithCaret: ' |'.
	interactionEngine smartCharacterWithEvent: (self keyboardEventFor: $<).
	self assert: editor text equals: ' '
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterWithEventWriteClosingBeforeSameCharacter [
	"Triggering a smart character inside the same smart character pair still works."

	self setEditorTextWithCaret: '|)'.
	editor keystroke: (self keyboardPressFor: $)).
	self assert: editor text equals: '))'.

	self setEditorTextWithCaret: '|]'.
	editor keystroke: (self keyboardPressFor: $]).
	self assert: editor text equals: ']]'.

	self setEditorTextWithCaret: '|}'.
	editor keystroke: (self keyboardPressFor: $}).
	self assert: editor text equals: '}}'.

	self setEditorTextWithCaret: '|"'.
	editor keystroke: (self keyboardPressFor: $").
	self assert: editor text equals: '"""'.

	self setEditorTextWithCaret: '|'''.
	editor keystroke: (self keyboardPressFor: $').
	self assert: editor text equals: ''''''''
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterWithEventWritePairAfterSameCharacter [
	"Triggering a smart character inside the same smart character pair still works."

	self setEditorTextWithCaret: '"|'.
	editor keystroke: (self keyboardPressFor: $").
	self assert: editor text equals: '"""'.

	self setEditorTextWithCaret: '''|'.
	editor keystroke: (self keyboardPressFor: $').
	self assert: editor text equals: ''''''''
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartCharacterWithEventWritePairInsideExistingPair [
	"Triggering a smart character inside the same smart character pair still works."

	self setEditorTextWithCaret: '(|)'.
	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $().
	self assert: editor text equals: '(())'.

	self setEditorTextWithCaret: '[|]'.
	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $[).
	self assert: editor text equals: '[[  ]]'.

	self setEditorTextWithCaret: '{|}'.
	editor keystroke: (self keyboardPressFor: ${).
	self assert: editor text equals: '{{  }}'.

	self setEditorTextWithCaret: '"|"'.
	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $").
	self assert: editor text equals: '""""'.

	"initial string: '' (two quotes) ; typing $' ; final string: ''' (three quotes, and the inserted quote is escaped)"
	self setEditorTextWithCaret: '''|'''.
	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $').
	self assert: editor text equals: ''''''''''
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartCharactersOppositeOf [

	self allSmartCharacters
		do: [ :pairs |
			self
				assert: (interactionEngine smartCharacterOppositeOf: pairs first ifAbsent: [ nil ])
				equals: pairs last ]
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartCharactersShouldClose [
	"Some smart characters shouldn't auto close, ohers not"
	self assert: (interactionEngine smartCharacterShouldClose: $() equals: true.
	self assert: (interactionEngine smartCharacterShouldClose: $<) equals: false
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartDoubleQuoteDoesNotEscapeQuote [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' te''xt ';
		selectFrom: 2 to: 6.

	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $").
	self assert: editor text equals: ' "te''xt" '
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartDoubleQuoteSurroundsEscapesSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' te"xt ';
		selectFrom: 2 to: 6.

	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $").
	self assert: editor text equals: ' "te""xt" '
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartDoubleQuoteSurroundsSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.

	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $").
	self assert: editor text equals: ' "text" '
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartNeedExtraRemoveInForOppositeAt [
	"index of cursor = 1 + index of character to remove"

	self setEditorTextWithCaret: '(|)'.
	self assert: interactionEngine smartBackspace. "result: ''"

	self setEditorTextWithCaret: '(()|'.
	self deny: interactionEngine smartBackspace. "result: '(('"

	self setEditorTextWithCaret: '(1(|)'.
	self assert: interactionEngine smartBackspace. "result: '(1'"

	self setEditorTextWithCaret: '(1(1(|))'.
	self assert: interactionEngine smartBackspace. "result: '(1(1)'"

	self setEditorTextWithCaret: '((1)1(|)))'.
	self assert: interactionEngine smartBackspace. "result: '((1)1))'"

	self setEditorTextWithCaret: '()((|)'.
	self deny: interactionEngine smartBackspace. "result: '()()'"

	self setEditorTextWithCaret: '(foobar(|)'.
	self assert: interactionEngine smartBackspace. "result: '(foobar'"

	self setEditorTextWithCaret: '[|]'.
	self assert: interactionEngine smartBackspace. "result: ''"

	self setEditorTextWithCaret: '{|}'.
	self assert: interactionEngine smartBackspace. "result: ''"

	self setEditorTextWithCaret: '"|"'.
	self assert: interactionEngine smartBackspace. "result: ''"

	self setEditorTextWithCaret: '''|'''.
	self assert: interactionEngine smartBackspace "result: ''"
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartNeedExtraRemoveInForOppositeAtNoRemoveForLiterals [
	"index of cursor = 1 + index of character to remove"

	self setEditorTextWithCaret: '($(|)'.
	self deny: interactionEngine smartBackspace. "result: '($)'"

	self setEditorTextWithCaret: '$[|]'.
	self deny: interactionEngine smartBackspace. "result: '$]'"

	self setEditorTextWithCaret: '$"|"'.
	self deny: interactionEngine smartBackspace
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartQuoteSurroundsDoesNotEscapeDoubleQuote [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' te"xt ';
		selectFrom: 2 to: 6.

	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $').
	self assert: editor text equals: ' ''te"xt'' '
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartQuoteSurroundsEscapesSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' te''xt ';
		selectFrom: 2 to: 6.

	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $').
	self assert: editor text equals: ' ''te''''xt'' '
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testSmartQuoteSurroundsSelection [
	"Test we surround selected text with smart characters properly"
	self
		setEditorText: ' text ';
		selectFrom: 2 to: 5.

	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $').
	self assert: editor text equals: ' ''text'' '
]

{ #category : 'tests - undo' }
CompletionEngineTest >> testUndoAutocompleteLeavesCursorInOriginalPosition [

	"If the caret is at the end of a word, replace the entire word"
	editor addString: 'self'.
	editor closeTypeIn.
	editor unselect.
	
	"Put the cursor after the `sel` token, and then we will simulate code completion"
	self selectAt: 'self' size - 1.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'selection'.
	
	editor undo.
	self expectText: 'self'.
	self assert: editor selectionInterval equals: (3 to: 2)
]

{ #category : 'tests - undo' }
CompletionEngineTest >> testUndoCompletionEntryKeepsFollowingLine [

	"If the caret is at the end of a word, replace the entire word"

	| text |
	text := 'self mEthOdThatDoes
nextLine'.

	self
		setEditorText: text;
		selectAt: text lines first size.

	editor textArea openInWorld.
	interactionEngine openMenu.

	interactionEngine context replaceTokenInEditorWith: 'mEthOdThatDoesNotExist'.

	editor undo.
	
	self assert: editor text asString equals: text
]

{ #category : 'tests - keyboard' }
CompletionEngineTest >> testWriteSmartCharacterPairInsideExistingPair [
	"Triggering a smart character inside the same smart character pair still works."

	self setEditorTextWithCaret: '(|)'.
	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $().
	self assert: editor text equals: '(())'.

	self setEditorTextWithCaret: '[|]'.
	interactionEngine smartCharacterWithEvent: (self keyboardPressFor: $[).
	self assert: editor text equals: '[[  ]]'
]
