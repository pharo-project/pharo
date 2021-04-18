"
I'm a version of CustomHelp that emit microdown instead of tweaking text.
"
Class {
	#name : #CustomHelp2,
	#superclass : #Model,
	#category : #'HelpSystem-Core-Utilities'
}

{ #category : #converting }
CustomHelp2 class >> asHelpTopic [
	"Convert the receiver to a help topic"
	<newSystemHelp>
	
	^self builder buildHelpTopicFrom: self
]

{ #category : #formatting }
CustomHelp2 class >> bold: aString [
	"Return Text object with bold emphasis."
	aString ifEmpty: [ self errorEmptyString ].
	^ aString asText addAttribute: TextEmphasis bold from: 1 to: aString size.

]

{ #category : #accessing }
CustomHelp2 class >> bookDescription [
	"return description of the book that will be visible when one selects the book node"
	^ ''
]

{ #category : #accessing }
CustomHelp2 class >> bookName [
	"Returns the name of the custom help book"
	
	^'Help'
]

{ #category : #defaults }
CustomHelp2 class >> builder [
	"Returns the builder that is used to build the given help book from the receiver. You can override this method
	 in a subclass to provide an own builder".
	
	^CustomHelpHelpBuilder
]

{ #category : #formatting }
CustomHelp2 class >> doItFrom: aString [
	"Return Text object with a specific color and mouse click action
	that executes aString; aString should also be a Smalltalk script that is possible to evaluate."
	
	^self doItFrom: aString text: aString

]

{ #category : #formatting }
CustomHelp2 class >> doItFrom: aString text: textString [
	"Return Text object with a specific color and mouse click action displayed as textString; 
	 aString should be a Smalltalk script that is possible to evaluate."
	| color |
	
	color := Color fromHexString: 'aa5693'.

	aString ifEmpty: [ self errorEmptyString ].
	^ textString asText
		addAttribute: (TextAction new actOnClickBlock: [
				Smalltalk compiler evaluate: aString]; yourself) 
			from: 1 to: textString size;
		addAttribute: (TextColor new color: color) from: 1 to: textString size;
		yourself

]

{ #category : #formatting }
CustomHelp2 class >> doItScriptFrom: aString [
	"Return Text object with a specific color and mouse click action
	that executes aString; aString should be a Smalltalk script that is possible to evaluate.
	After the script, a copy to clipboard and run action appears."

	| color text clipboard run |
	color := Color fromHexString: 'aa5693'.
	aString ifEmpty: [ self errorEmptyString ].
	text := aString asText
		addAttribute: (TextColor new color: color);
		yourself.
	clipboard := self newColoredText: color withClipboardAction: aString.
	run := self newColoredText: color withRunAction: aString.
	^ text , '  ' asText , clipboard, '  ' asText, run
]

{ #category : #errors }
CustomHelp2 class >> errorEmptyString [

	self error: 'Expected a non empty string'
]

{ #category : #formatting }
CustomHelp2 class >> heading: aString [
	"Return Text object with heading formating attributes."

	^ '# ', aString, Character cr asString
]

{ #category : #accessing }
CustomHelp2 class >> icon [
	"Returns an icon used for displaying the custom help book"

	^ self iconNamed: #bookIcon
]

{ #category : #testing }
CustomHelp2 class >> isUsed [
	"all my sublasses are used"
	^self name = 'CustomHelp'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]
]

{ #category : #accessing }
CustomHelp2 class >> key [
	"Returns a unique key identifying the receiver in the help system"
	
	^''
]

{ #category : #'instance creation' }
CustomHelp2 class >> newColoredText: color withClipboardAction: aString [
	"Return new Text object with copy icon and copy to clipboard action."
	| text icon |
	icon := Text
		string: (String value: 1)
		attributes:
			{(TextAnchor new anchoredMorph: (self iconNamed: #smallCopy)).
			(TextAction new
				actOnClickBlock: [ Clipboard clipboardText: aString.
					self inform: 'The URL has been pasted into your clipboard' ])}.
	text := ' (copy to clipboard)' asText
		addAttribute:
			(TextAction new
				actOnClickBlock: [ Clipboard clipboardText: aString.
					self inform: 'The URL has been pasted into your clipboard' ]);
		addAttribute: (TextColor new color: color);
		addAttribute: TextEmphasis italic;
		yourself.
	^ icon, text
]

{ #category : #'instance creation' }
CustomHelp2 class >> newColoredText: color withRunAction: aString [
	"Return new Text object with copy icon and run action."
	| run icon |
	icon := Text
		string: (String value: 1)
		attributes: { 
			(TextAnchor new anchoredMorph: (self iconNamed: #glamorousGo)).
			(TextAction new actOnClickBlock: [
				Smalltalk compiler evaluate: aString]; yourself) }.
	run := ' (execute it)' asText
		addAttribute:
			(TextAction new actOnClickBlock: [
				Smalltalk compiler evaluate: aString]; yourself);
		addAttribute: (TextColor new color: color);
		addAttribute: TextEmphasis italic;
		yourself.
	^ icon, run
]

{ #category : #accessing }
CustomHelp2 class >> pages [ 
	"Returns a collection of method selectors to return the pages of the custom help book"
		
	^#()
]

{ #category : #formatting }
CustomHelp2 class >> subheading: aString [
	"Return Text object with subheading formating attributes."

	^ '## ', aString, Character cr asString
]

{ #category : #accessing }
CustomHelp2 class >> systemIcon [ 

	^self icon
]

{ #category : #formatting }
CustomHelp2 class >> url: aString [
	"Return Text object with copy aString to the clipboard after mouse click."
	| clickBlock |
	
	aString ifEmpty: [ self errorEmptyString ].
	
	clickBlock := [ WebBrowser openOn: aString ].

	^ aString asText 
		addAttribute: (	TextAction new actOnClickBlock: clickBlock);
		addAttribute: (TextColor new color: self theme urlColor);
		yourself

]
