formatting
doItFrom: aString

	| color |
	
	color := Color fromHexString: 'aa5693'.

	aString size = 0 ifTrue: [^self].
	^ aString asText
		addAttribute: (TextAction new actOnClickBlock: [
				Smalltalk compiler evaluate: aString]; yourself) 
			from: 1 to: aString size;
		addAttribute: (TextColor new color: color) from: 1 to: aString size;
		yourself
