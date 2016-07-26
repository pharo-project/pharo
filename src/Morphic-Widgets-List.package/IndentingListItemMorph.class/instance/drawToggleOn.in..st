drawing
drawToggleOn: aCanvas in: aRectangle

	| aForm centeringOffset |
	complexContents hasContents ifFalse: [^self].
	aForm := isExpanded 
		ifTrue: [container expandedFormForMorph: self]
		ifFalse: [container notExpandedFormForMorph: self].
	centeringOffset := ((aRectangle height - aForm extent y) / 2.0) truncated.
	^aCanvas 
		translucentImage: aForm 
		at: (aRectangle topLeft translateBy: 0 @ centeringOffset).
