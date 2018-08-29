"
I represent the mode to use for editing smalltalk code
"
Class {
	#name : #RubSmalltalkCodeMode,
	#superclass : #RubAbstractSmalltalkMode,
	#instVars : [
		'classOrMetaclass'
	],
	#category : #'Rubric-Editing-Code'
}

{ #category : #menu }
RubSmalltalkCodeMode class >> editingModeMenuOn: aBuilder [
	"Specify the menu used to select an editing mode"

	<contextMenu>
	<RubEditingModeMenu>
	(aBuilder item: self label translated)
		action: [:model | model textArea editingMode: self new]
]

{ #category : #accessing }
RubSmalltalkCodeMode class >> label [
	^ 'Smalltalk code'
]

{ #category : #menu }
RubSmalltalkCodeMode class >> menuKeyword [
	^ 'RubSmalltalkCodeMenu'
]

{ #category : #menu }
RubSmalltalkCodeMode class >> menuOn: aBuilder [
	"Specify the menu used when writing code."

	<contextMenu>
	<RubSmalltalkCodeMenu>
	(aBuilder item: #'Do it' translated)
		order: 1;
		keyText: 'd';
		selector: #doIt;
		help: nil;
		iconName: #smallDoItIcon.
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		help: nil;
		iconName: #smallPrintItIcon.
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		help: nil;
		iconName: #smallInspectItIcon.
	(aBuilder item: #'Basic Inspect it' translated)
		keyText: 'I';
		selector: #basicInspectIt;
		help: nil;
		iconName: #smallInspectItIcon.
	(aBuilder item: #'Debug it' translated)
		keyText: 'D';
		selector: #debugIt;
		help: nil;
		iconName: #smallDebugIcon.
	(aBuilder item: #'Profile it' translated)
		selector: #tallyIt;
		iconName: #smallDebugIcon;
		withSeparatorAfter.
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		help: nil;
		iconName: #smallFindIcon.
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		help: nil;
		iconName: #smallFindIcon.
	self extendedSearchMenuOn: aBuilder.
	(aBuilder item: #'Redo' translated)
		keyText: 'Z';
		selector: #redo;
		help: nil;
		iconName: #smallRedoIcon.
	(aBuilder item: #Undo translated)
		keyText: 'z';
		selector: #undo;
		help: nil;
		iconName: #smallUndoIcon;
		withSeparatorAfter.
	(aBuilder item: #Copy translated)
		keyText: 'c';
		selector: #copySelection;
		help: nil;
		iconName: #smallCopyIcon.
	(aBuilder item: #Cut translated)
		keyText: 'x';
		selector: #cut;
		help: nil;
		iconName: #smallCutIcon.
	(aBuilder item: #Paste translated)
		keyText: 'v';
		selector: #paste;
		help: nil;
		iconName: #smallPasteIcon.
	(aBuilder item: #'Paste...' translated)
		selector: #pasteRecent;
		help: nil;
		iconName: #smallCopyIcon;
		withSeparatorAfter.
	aBuilder model
		ifNotNil: [ :mod | 
			mod model
				ifNotNil: [ (aBuilder item: #Accept translated)
						keyText: 's';
						selector: #accept;
						iconName: #smallOkIcon.
					(aBuilder item: #Cancel translated)
						keyText: 'l';
						selector: #cancel;
						iconName: #smallCancelIcon ] ]
]

{ #category : #parsing }
RubSmalltalkCodeMode >> bestNodeInSource: source at: selectionInterval shouldFavourExpressions: isFavouringExpressions onError: aBlock [
	"Find the best node in the source text in the identified selection area"
	
	| ast node start stop |

	start := selectionInterval first min: source size.
	stop := selectionInterval last min: source size.
	
	ast := self parseSource: source.
	
	ast ifNotNil: [ 
		"If there is no text selection, try and select something intelligent"
		((selectionInterval size < 2) and: [ isFavouringExpressions ]) ifTrue: [ 
			"If there is white space or statement terminator, try to backup to find a better node"		
			[stop > 0 and: [('.;' includes: (source at: stop)) or: [(source at: stop) isSeparator ]]]
				whileTrue: [ start := stop := stop - 1 ]].
		
		node := ast bestNodeFor: (start to: stop).
		
		node ifNil: [ 
			node := RBParseErrorNodeVisitor
				visit: ast 
				detect: [ :n | n intersectsInterval: (start to: start)]
				ifNone: [ ^aBlock value ]].
		
		node isFaulty ifTrue: [ 
			node allChildren size = 1 ifTrue: [ 
				(self parseExpression: (node value padLeftTo: source size)) 
					ifNil: [ ^node ] 
					ifNotNil: [ :newNode |
						ast replaceNode: node withNode: newNode.
						node := newNode ].
				].
			
			node := RBGenericNodeVisitor 
				visit: node 
				detect: [ :n | n intersectsInterval: (start to: start) ] 
				ifNone: [ node ] ] ].
	

	^node

]

{ #category : #shout }
RubSmalltalkCodeMode >> bindingOf: varName [
	^ self model bindingOf: varName
]

{ #category : #shout }
RubSmalltalkCodeMode >> classOrMetaClass: aBehavior [
	"set the classOrMetaClass in the receiver's styler to aBehavior"
	classOrMetaclass := aBehavior.
	self updateStyler
]

{ #category : #'keyboard navigation' }
RubSmalltalkCodeMode >> findNextKeywordIn: src startingAt: selectionInterval searchingForward: isFwd ifFound: textSelectionBlock [
	"Find the next keyword/useful source location to then apply a @textSelectionBlock to.
	Anser the result of the @textSelectionBlock"
	
	| checkBlock ranges potentialLocation selectionEnd|
	
	"match on typical locations you jump to in source to complete typing. We use regex to better cope
	with broken source code (where its very handy to jump while fixing code) - and get initial 
	location hints which can then use an ast to infer on"
	
	ranges := src allRangesOfRegexMatches: 
		'\[\s*|', "[ Iself msg1... ]"
		'\(\s*|', "( I5 + 2..."
		'\:=\s*|', "x := Iself ..."
		'\S+\:\s|', "ifTrue1: I...."
		'\|\s*|', "[ :var | Iself...]"
		'\:\S+|', "[ :var :var2 | ..."
		'\;\s+|', " self msg1; Imsg2...." 
		'\,\s*|', " 'string1', I'string 2'...."
		'\)|', " 4 + 5)I ifTrue..."
		'\]|', " true and: [ true ]I )..."
		'\.\s+'.  "self msg1. Iself msg2..."

	selectionEnd := selectionInterval last.
	isFwd
		ifTrue: [ checkBlock := [ :match | match last > selectionEnd ] ]
		ifFalse: [ ranges := ranges reversed.
			checkBlock := [ :match | match last <= (selectionEnd - (match last - match first + 1)) ] ].
		
	ranges
		do: [ :match | 
			(checkBlock value: match)
				ifTrue: [ 
					potentialLocation := match last.
					self
						findValidKeywordIn: src
						at: potentialLocation
						ifFound: [ 
								^textSelectionBlock value: potentialLocation] ] ].

		
	"If nothing found, then jump to end of any current selection"
	selectionInterval isEmpty ifFalse: [ 
		textSelectionBlock value: selectionEnd ]
]

{ #category : #'keyboard navigation' }
RubSmalltalkCodeMode >> findValidKeywordIn: sourceCode at: locationIndex ifFound: aBlock [
	"find a valid keyword in the source, starting at @locationIndex. If a suitable node is found,
	evaluate the Block"
	
	| node validLocation locationInterval |
	
	validLocation := true.
	locationInterval := locationIndex to: locationIndex.
	
	node := self
		bestNodeInSource: sourceCode
		at: locationInterval
		shouldFavourExpressions: false
		onError: [ nil ].
		
	node
		ifNotNil: [ 
			node isLiteralNode
				ifTrue: [ validLocation := false ].
			node nodesDo: [ :subNode |
				subNode comments
					detect: [ :c | c intersectsInterval: locationInterval ]
					ifFound: [ validLocation := false ] ] ].
			
	validLocation ifTrue: aBlock 
			
]

{ #category : #'initialize-release' }
RubSmalltalkCodeMode >> formatMethodCode [
	| source tree formatted |
	source := self textArea text asString.
	tree := RBParser parseMethod: source onError: [ :msg :pos | ^ self ].
	formatted := tree formattedCodeWithMaxLineLength: self maxFormatterLineLength.
	formatted = source
		ifTrue: [ ^ self ].
	self textArea updateTextWith: formatted
]

{ #category : #shout }
RubSmalltalkCodeMode >> hasBindingOf: aString [
	" For the shout styler "
	^ self model notNil and: [ self model hasBindingOf: aString ]
]

{ #category : #shout }
RubSmalltalkCodeMode >> hasBindingThatBeginsWith: aString [ 
	" For the shout styler "
	^ self model notNil and: [ self model hasBindingThatBeginsWith: aString ]
]

{ #category : #formatting }
RubSmalltalkCodeMode >> maxFormatterLineLength [
	^ self textArea wrapped
		ifTrue: [ (self textArea innerBounds width / 9) integerPart ]
		ifFalse: [ 70 ]
]

{ #category : #parsing }
RubSmalltalkCodeMode >> parseExpression: aString [

	^RBParser parseFaultyExpression: aString
]

{ #category : #parsing }
RubSmalltalkCodeMode >> parseSource: aString [

	^RBParser parseFaultyMethod: aString
]

{ #category : #shout }
RubSmalltalkCodeMode >> shoutAboutToStyle: myShoutStyler [
	^ self model isNil
		or: [ 
			(self model interactionModel respondsTo: #shoutAboutToStyle:) not
				or: [ self model interactionModel shoutAboutToStyle: self ] ] 
]

{ #category : #shout }
RubSmalltalkCodeMode >> shoutStyler [
	^ self textArea decoratorNamed: #shoutStyler
]

{ #category : #shout }
RubSmalltalkCodeMode >> updateStyler [

	self textArea ifNil: [ ^ self ].
	self textArea shoutStyler ifNotNil: [ :s | s classOrMetaClass: classOrMetaclass ]
]

{ #category : #'initialize-release' }
RubSmalltalkCodeMode >> updateTextAreaWhenPlugged [
	self withShoutStyler.
	super updateTextAreaWhenPlugged


]

{ #category : #'initialize-release' }
RubSmalltalkCodeMode >> updateTextAreaWhenUnplugged [
	self textArea withoutShoutStyler.
	super updateTextAreaWhenUnplugged


]

{ #category : #'keyboard navigation' }
RubSmalltalkCodeMode >> widenSelectionIn: sourceCode selection: selectionInterval ifFound: selectionBlock [
	"Extend the current highlight selection using knowledge of the AST and cursor location"
	| node start stop comments cursorIndex |
	
	node := self bestNodeInSource: sourceCode at: selectionInterval shouldFavourExpressions: true onError: [ ^nil ].
	
	cursorIndex := selectionInterval last.
	selectionInterval ifNotEmpty: [ 
		node parent ifNotNil: [ node := node parent ].
		node isSequence ifTrue: [ node := node parent]].
	
	start := node start.
	stop := node stop max: cursorIndex - 1.
	
	comments := OrderedCollection new.
	RBCommentNodeVisitor visit: node do: [ :cmnts | comments add: cmnts ].

	comments 
		detect: [ :c | c start < cursorIndex and: [ c stop >= cursorIndex ] ] 
		ifFound: [ :c | start := c start. stop := c stop ].
	
	"Handle cascaded messages"
	node isMessage ifTrue: [
		(node isCascaded) 
			ifTrue: [ | i cascadeStart|
				(i := node parent messages indexOf: node) > 1 
					ifTrue: [ 
						cascadeStart := (node parent semicolons at: i - 1).
						cascadeStart < cursorIndex ifTrue: [ start := start min: cascadeStart ]]]
			ifFalse: [
				"Try to select the first chained message send - experimental"
				"self hasSelection ifFalse: [ 
					start := node receiver stop + 1.
					node hasParentheses ifTrue: [stop := stop - 1]]"]].
			
	"textMorph selectionInterval: (start to: stop)"
	selectionBlock value: (start to: stop)
]

{ #category : #shout }
RubSmalltalkCodeMode >> withShoutStyler [
	self textArea withDecoratorNamed: #shoutStyler.
	self updateStyler.
]

{ #category : #shout }
RubSmalltalkCodeMode >> withoutShoutStyler [
	self textArea withoutDecoratorNamed: #shoutStyler
]
