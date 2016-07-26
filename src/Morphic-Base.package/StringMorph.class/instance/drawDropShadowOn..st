drawing
drawDropShadowOn: aCanvas

	| bnd gap |
	bnd := self bounds translateBy: self shadowOffset.
	gap := self layoutInset.
	bnd := (bnd topLeft + gap) corner: (bnd bottomRight + gap).
	aCanvas drawString: contents in: bnd font: self fontToUse color: self shadowColor.