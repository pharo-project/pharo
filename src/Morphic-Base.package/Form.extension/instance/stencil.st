*Morphic-Base-Support
stencil
	"return a 1-bit deep, black-and-white stencil of myself"

	| canvas |
	canvas := FormCanvas extent: self extent depth: 1.
	canvas fillColor: (Color white).

	canvas stencil: self at: 0@0  
				sourceRect: (Rectangle origin: 0@0 corner: self extent) color: Color black.

	^ canvas form
