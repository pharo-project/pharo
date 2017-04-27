instance creation
newColoredText: color withRunAction: aString
	"Return new Text object with copy icon and run action."
	| run icon |
	icon := Text
		string: (String value: 1)
		attributes: { 
			(TextAnchor new anchoredMorph: #glamorousGo asIcon).
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