drawing
drawOn: aCanvas
	| text paragraph keyBounds keyFont |

	text := self text.
	keyFont := self class symbolFont.

	keyBounds := self boundsForKeyText: text string font: keyFont.
	paragraph := Paragraph new
		compose: text
			style: (TextStyle fontArray: { keyFont })
			from: 1
			in: (0@0 corner: keyBounds corner);
		yourself.
		
	aCanvas 
		paragraph: paragraph 
		bounds: keyBounds 
		color: self owner stringColorToUse