accessing
borderStyle: aBorderStyle 
	"Work around the borderWidth/borderColor pair"

	aBorderStyle = self borderStyle ifTrue: [^self].
	self assureExtension.
	"secure against invalid border styles"
	(self canDrawBorder: aBorderStyle) 
		ifFalse: 
			["Replace the suggested border with a simple one"

			^self borderStyle: (BorderStyle width: aBorderStyle width
						color: (aBorderStyle trackColorFrom: self) color)].
	aBorderStyle width = self borderStyle width ifFalse: [self changed].
	(aBorderStyle isNil or: [aBorderStyle == BorderStyle default]) 
		ifTrue: 
			[extension borderStyle: nil.
			borderWidth := 0.
			^self changed].
	extension borderStyle: aBorderStyle.
	borderWidth := aBorderStyle width.
	borderColor := aBorderStyle style == #simple 
				ifTrue: [aBorderStyle color]
				ifFalse: [aBorderStyle style].
	self changed