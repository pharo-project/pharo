private
addNameBeneath: outerRectangle string: aString
	"Add a name display centered beneath the bottom of the outer rectangle. Return the handle."

	| namePosition w |
	w := self world ifNil:[target world].
	
	nameMorph := StringMorph contents: aString font: StandardFonts haloFont.
	nameMorph 
	wantsYellowButtonMenu: false;
	color: Color black;
	backgroundColor: Color white;
	target: innerTarget.
	
	namePosition := outerRectangle bottomCenter -
		((nameMorph width // 2) @ (self handleSize negated // 2 - 1)).
		
	nameMorph position: (namePosition min: w viewBox bottomRight - nameMorph extent y + 2).
	self addMorph: nameMorph.
	^ nameMorph