Extension { #name : #VTermOutputDriver }

{ #category : #'*System-CommandLine-TextSupport' }
VTermOutputDriver >> background256: aColor [
	"output an xterm 256 color extension sequence"
	self color256: aColor background: true
]

{ #category : #'*System-CommandLine-TextSupport' }
VTermOutputDriver >> color256: aColor [
	"output an xterm 256 color extension sequence."
	self color256: aColor background: false
]

{ #category : #'*System-CommandLine-TextSupport' }
VTermOutputDriver >> color256: aColor background: isBackground [
	"use the resources for colors 0-15 - usually more-or-less a reproduction of the standard ANSI colors, but possibly more pleasing shades
	colors 16-231 are a 6x6x6 color cube
	colors 232-255 are a grayscale ramp, intentionally leaving out black and white"
	
	self csiEscape.

	outStream nextPutAll: (isBackground 
		ifFalse: [ '38;5;' ]
		ifTrue:  [ '48;5;' ]).
		
	outStream 
		print: aColor closestXTermPixelValue;
		nextPut: $m.
]

{ #category : #'*System-CommandLine-TextSupport' }
VTermOutputDriver class >> printText: aText [
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
]
