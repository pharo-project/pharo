Class {
	#name : #CustomHelpTextFormatter,
	#superclass : #Model,
	#category : #'HelpSystem-Core-Utilities'
}

{ #category : #formatting }
CustomHelpTextFormatter >> bold: aString [
	"Return Text object with bold emphasis."
	aString ifEmpty: [ self errorEmptyString ].
	^ aString asText addAttribute: TextEmphasis bold from: 1 to: aString size.
]

{ #category : #formatting }
CustomHelpTextFormatter >> doItFrom: aString [
	"Return Text object with a specific color and mouse click action
	that executes aString; aString should also be a Smalltalk script that is possible to evaluate."
	
	^self doItFrom: aString text: aString
]

{ #category : #formatting }
CustomHelpTextFormatter >> doItFrom: aString text: textString [
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
CustomHelpTextFormatter >> heading: aString [
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
CustomHelpTextFormatter >> heading: aString withSubtitle: aSubtitleString [
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

{ #category : #'instance creation' }
CustomHelpTextFormatter >> newColoredText: color withClipboardAction: aString [
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
CustomHelpTextFormatter >> newColoredText: color withRunAction: aString [
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

{ #category : #formatting }
CustomHelpTextFormatter >> sourceCode: aSourceCode [

	| font text |
	font := (LogicalFont familyName: 'Source Code Pro' pointSize: 10).
	text := aSourceCode asText.
	
	^ text
		addAttribute: (TextFontReference toFont: font) from: 3 to: text size - 1;
		yourself
]

{ #category : #formatting }
CustomHelpTextFormatter >> subheading: aString [
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

{ #category : #formatting }
CustomHelpTextFormatter >> subsubheading: aString [
	"Return Text object with subheading formating attributes."

	| text font color |
	
	font := (LogicalFont familyName: 'Source Sans Pro' pointSize:12).
	color := Color fromHexString: '3196D3'.
	text := (String cr asText, String cr, aString, String cr, String cr).
	
	^ text
		addAttribute: (TextFontReference toFont: font) from: 3 to: text size - 1;
		addAttribute: TextEmphasis bold from: 1 to: text size - 1;
		addAttribute: (TextColor new color: color) from: 1 to: text size - 1;
		yourself
]

{ #category : #formatting }
CustomHelpTextFormatter >> underlined: aString [
	"Return Text object with underlined emphasis."
	aString ifEmpty: [ self errorEmptyString ].
	
	^ aString asText addAttribute: TextEmphasis underlined from: 1 to: aString size.
]

{ #category : #formatting }
CustomHelpTextFormatter >> url: aString [
	"Return Text object with copy aString to the clipboard after mouse click."
	| clickBlock |
	
	aString ifEmpty: [ self errorEmptyString ].
	
	clickBlock := [ WebBrowser openOn: aString ].

	^ aString asText 
		addAttribute: (	TextAction new actOnClickBlock: clickBlock);
		addAttribute: (TextColor new color: self theme urlColor);
		yourself
]
