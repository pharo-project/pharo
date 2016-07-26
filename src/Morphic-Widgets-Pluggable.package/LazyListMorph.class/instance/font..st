drawing
font: newFont
	font := (newFont ifNil: [ TextStyle default defaultFont ]).
	self adjustHeight.
	self changed.