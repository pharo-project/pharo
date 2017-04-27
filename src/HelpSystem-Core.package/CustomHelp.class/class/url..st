formatting
url: aString
	"Return Text object with copy aString to the clipboard after mouse click."
	| color |
	
	color := Color fromHexString: '2176B3'.

	aString size = 0 ifTrue: [^self].
	^ aString asText
		addAttribute: (TextAction new actOnClickBlock: [
				Clipboard clipboardText: aString. 
				self inform: 'The URL has been pasted into your clipboard']) 
			from: 1
			to: aString size;
		addAttribute: (TextColor new color: color) from: 1 to: aString size;
		yourself
