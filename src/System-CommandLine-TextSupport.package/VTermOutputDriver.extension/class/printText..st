*System-CommandLine-TextSupport
printText: aText
	" Example code shows how to output formatted text"
	| out |

	out := self stdout.

	aText doWithIndex: [ :char :index| 
		aText attributesAt: index do: [ :attr| 
			(attr isKindOf: TextEmphasis )	ifTrue: [ out bold ].
			(attr isKindOf: TextColor )	ifTrue: [ out color256: attr color ]].
		(char = Character cr)
			ifTrue: [ out lf ]
			ifFalse: [ out << char ].
		out clear ].

	out lf.