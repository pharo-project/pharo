formatting
url: aString
	"Return Text object with copy aString to the clipboard after mouse click."
	| color clickBlock |
	
	aString ifEmpty: [ self errorEmptyString ].
	
	color := Color fromHexString: '3196D3'.
	
	clickBlock := [ WebBrowser openOn: aString ].

	^ aString asText 
		addAttribute: (	TextAction new actOnClickBlock: clickBlock);
		addAttribute: (TextColor new color: color);
		yourself
