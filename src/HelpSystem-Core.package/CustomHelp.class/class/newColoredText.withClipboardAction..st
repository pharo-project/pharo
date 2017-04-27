instance creation
newColoredText: color withClipboardAction: aString
	"Return new Text object with copy icon and copy to clipboard action."
	| text icon |
	icon := Text
		string: (String value: 1)
		attributes:
			{(TextAnchor new anchoredMorph: #smallCopy asIcon).
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