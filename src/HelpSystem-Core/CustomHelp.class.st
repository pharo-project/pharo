"
This is a common superclass for custom help. Subclasses of this class are automatically included into the system help.

By default the informations provided on the receiver class are converted into help topics by a specific builder 
- here the CustomHelpHelpBuilder. Note that you can provide an own custom builder by overriding the #builder method


"
Class {
	#name : #CustomHelp,
	#superclass : #Model,
	#category : #'HelpSystem-Core-Utilities'
}

{ #category : #editing }
CustomHelp class >> accept: aSelector title: title contents: text [
	"Accept edited text. Compile it into a HelpTopic"

	| code |
	code := String streamContents:[:s|
		s nextPutAll: aSelector.
		s crtab; nextPutAll: '"This method was automatically generated."'.
		s crtab; nextPutAll: '<script: ''self edit: ', aSelector storeString,'''>'.
		s crtab; nextPutAll: '^HelpTopic'.
		s crtab: 2; nextPutAll: 'title: ', title storeString.
		s crtab: 2; nextPutAll: 'contents: '.
		s cr; nextPutAll: (String streamContents:[:c| c nextChunkPut: text]) storeString.
		s nextPutAll:' readStream nextChunkText'.
	].

	self class 
		compile: code
		classified: ((self class organization categoryOfElement: aSelector) ifNil:['pages']).

]

{ #category : #converting }
CustomHelp class >> asHelpTopic [
	"Convert the receiver to a help topic"
	<systemHelp>
	
	^self builder buildHelpTopicFrom: self
]

{ #category : #formatting }
CustomHelp class >> bold: aString [
	"Return Text object with bold emphasis."
	aString ifEmpty: [ self errorEmptyString ].
	^ aString asText addAttribute: TextEmphasis bold from: 1 to: aString size.

]

{ #category : #accessing }
CustomHelp class >> bookDescription [
	"return description of the book that will be visible when one selects the book node"
	^ ''
]

{ #category : #accessing }
CustomHelp class >> bookName [
	"Returns the name of the custom help book"
	
	^'Help'
]

{ #category : #defaults }
CustomHelp class >> builder [
	"Returns the builder that is used to build the given help book from the receiver. You can override this method
	 in a subclass to provide an own builder".
	
	^CustomHelpHelpBuilder
]

{ #category : #formatting }
CustomHelp class >> doItFrom: aString [
	"Return Text object with a specific color and mouse click action
	that executes aString; aString should also be a Smalltalk script that is possible to evaluate."
	
	^self doItFrom: aString text: aString

]

{ #category : #formatting }
CustomHelp class >> doItFrom: aString text: textString [
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
CustomHelp class >> doItScriptFrom: aString [
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

{ #category : #editing }
CustomHelp class >> edit: aSelector [
	"Open a Workspace on the text in the given selector.
	When accepted, compile the result as a help topic."

	| topic |
	topic := (self respondsTo: aSelector) 
			ifTrue:[self perform: aSelector]
			ifFalse:[HelpTopic title: 'Untitled' contents: 'Please edit this topic. 
To change the topic title, edit the window label.'].
	UIManager default
		edit: topic contents
		label: topic title
		accept: [:text| 
			|newTitle|
			newTitle := UIManager default request: 'Save with title ' initialAnswer: topic title.		 
			newTitle ifNotNil: [ self accept: aSelector title: newTitle contents: text ]].

]

{ #category : #errors }
CustomHelp class >> errorEmptyString [

	self error: 'Expected a non empty string'
]

{ #category : #formatting }
CustomHelp class >> heading: aString [
	"Return Text object with heading formating attributes."

	| font color |
	
	font := (LogicalFont familyName: 'Source Sans Pro' pointSize: 20).
	color := Color fromHexString: '3196D3'.

	aString ifEmpty: [ self errorEmptyString ].
	^ (aString asText, String cr, String cr)
		addAttribute: (TextFontReference toFont: font) from: 1 to: aString size;
		addAttribute: TextEmphasis bold from: 1 to: aString size;
		addAttribute: (TextColor new color: color) from: 1 to: aString size;
		yourself

]

{ #category : #formatting }
CustomHelp class >> heading: aString withSubtitle: aSubtitleString [
	"Return Text object with heading formating attributes."

	| font color |
	
	font := (LogicalFont familyName: 'Source Sans Pro' pointSize: 20).
	color := Color fromHexString: '3196D3'.

	aString ifEmpty: [ self errorEmptyString ].
	^ (aString asText, String cr, aSubtitleString, String cr, String cr)
		addAttribute: (TextFontReference toFont: font) from: 1 to: aString size;
		addAttribute: TextEmphasis bold from: 1 to: aString size;
		addAttribute: (TextColor new color: color) from: 1 to: aString size;
		addAttribute: (TextColor new color: color) from: aString size+1 to: aString size+1+aSubtitleString size;
		yourself

]

{ #category : #accessing }
CustomHelp class >> icon [
	"Returns an icon used for displaying the custom help book"

	^ self iconNamed: #bookIcon
]

{ #category : #testing }
CustomHelp class >> isUsed [
	"all my sublasses are used"
	^self name = 'CustomHelp'
		ifTrue: [ super isUsed ]
		ifFalse: [ true ]
]

{ #category : #accessing }
CustomHelp class >> key [
	"Returns a unique key identifying the receiver in the help system"
	
	^''
]

{ #category : #'instance creation' }
CustomHelp class >> newColoredText: color withClipboardAction: aString [
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
CustomHelp class >> newColoredText: color withRunAction: aString [
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
CustomHelp class >> pages [ 
	"Returns a collection of method selectors to return the pages of the custom help book"
		
	^#()
]

{ #category : #formatting }
CustomHelp class >> subheading: aString [
	"Return Text object with subheading formating attributes."

	| text font color |
	
	font := (LogicalFont familyName: 'Source Sans Pro' pointSize:16).
	color := Color fromHexString: '3196D3'.
	text := (String cr asText, String cr, aString, String cr, String cr).
	
	^ text
		addAttribute: (TextFontReference toFont: font) from: 3 to: text size - 1;
		addAttribute: TextEmphasis bold from: 1 to: text size - 1;
		addAttribute: (TextColor new color: color) from: 1 to: text size - 1;
		yourself

]

{ #category : #accessing }
CustomHelp class >> systemIcon [ 

	^self icon
]

{ #category : #formatting }
CustomHelp class >> underlined: aString [
	"Return Text object with underlined emphasis."
	aString ifEmpty: [ self errorEmptyString ].
	
	^ aString asText addAttribute: TextEmphasis underlined from: 1 to: aString size.

]

{ #category : #formatting }
CustomHelp class >> url: aString [
	"Return Text object with copy aString to the clipboard after mouse click."
	| clickBlock |
	
	aString ifEmpty: [ self errorEmptyString ].
	
	clickBlock := [ WebBrowser openOn: aString ].

	^ aString asText 
		addAttribute: (	TextAction new actOnClickBlock: clickBlock);
		addAttribute: (TextColor new color: self theme urlColor);
		yourself

]
