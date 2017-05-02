formatting
url: aString
	"Return Text object with copy aString to the clipboard after mouse click."
	| color clickBlock |
	
	aString ifEmpty: [ self errorEmptyString ].
	
	color := Color fromHexString: '2176B3'.
	
	clickBlock := [ 
		self environment 
			at: #WebBrowser 
			ifPresent: [ :browserClass | browserClass openOn: aString ]
			ifAbsent: [ 
				Clipboard clipboardText: aString.
				self inform: 'The URL has been pasted into your clipboard'.
			]
	].

	^ aString asText 
		addAttribute: (	TextAction new actOnClickBlock: clickBlock);
		addAttribute: (TextColor new color: color);
		yourself
