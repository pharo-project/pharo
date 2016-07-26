as yet unclassified
scan: aText for: scanner
	" scan text from starting index,
	answer the index of first character for potential next line"
	| index top |	

	text := aText.
	scanner text: text.

	"edge case, text is empty "
	text isEmpty ifTrue: [ ^ scanner ].
	
	scanner newLine: 1.
	index := 1.

	text runs withStartStopAndValueDo: [:start :stop :values |
		
		scanner setAttributes: values.
		start to: stop do: [:i |  | ch |
			ch := text at: i.
			ch = Character cr 
				ifTrue: [ 
					scanner endLine: i - 1.
					scanner newLine: i + 1 ]
				ifFalse: [
					scanner addCharacter: i.
				]
			]
	].
	scanner endLine: (text size).

	lines := scanner lines.

	"set the lines top offset"
	top := 0.
	lines do: [:line |
		line top: top.
		top := top + line height ].
	
