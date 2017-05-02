formatting
doItScriptFrom: aString
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