Extension { #name : #ToggleMenuItemShortcutWithSymbol }

{ #category : #'*Athens-Morphic' }
ToggleMenuItemShortcutWithSymbol >> drawOnAthensCanvas: aCanvas [
	| keyParts text paragraph keyBounds keyFont |

	keyParts := (self keyText substrings: '+') collect: #trimBoth.
	
	"Ensure shift is first"
	((keyParts includes: 'Shift') and: [ (keyParts first = 'Shift') not ])
		ifTrue: [ keyParts := (keyParts copyWithout: 'Shift') copyWithFirst: 'Shift' ].

	keyFont := self class symbolFont.

	text := keyParts 
		inject: Text new
		into: [ :s :each | 
			s, (self class 
				symbolTableAt: each
				ifAbsent: [ each capitalized ] )].

	keyBounds := self boundsForKeyText: text string font: keyFont.
	paragraph := Paragraph new
		compose: text
			style: (TextStyle fontArray: { keyFont })
			from: 1
			in: (0@0 corner: keyBounds corner);
		yourself.
	aCanvas asCanvasWrapper 
		paragraph: paragraph 
		bounds: keyBounds 
		color: self color
]
