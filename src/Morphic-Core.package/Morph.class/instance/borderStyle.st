accessing
borderStyle

	extension ifNil: [^BorderStyle default trackColorFrom: self].
	^(extension borderStyle ifNil: [BorderStyle default]) trackColorFrom: self