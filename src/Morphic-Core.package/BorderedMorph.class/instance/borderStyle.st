accessing
borderStyle
	"Work around the borderWidth/borderColor pair"

	| style |
	borderColor ifNil: [^BorderStyle default].
	borderWidth isZero ifTrue: [^BorderStyle default].
	self assureExtension.
	style := extension borderStyle ifNil: [BorderStyle default].
	(borderWidth = style width and: 
			["Hah! Try understanding this..."

			borderColor == style style or: 
					["#raised/#inset etc"

					#simple == style style and: [borderColor = style color]]]) 
		ifFalse: 
			[style := borderColor isColor 
				ifTrue: [BorderStyle width: borderWidth color: borderColor]
				ifFalse: [(BorderStyle perform: borderColor) width: borderWidth	"argh."].
			extension borderStyle: style].
	^style trackColorFrom: self