drawing
drawTitleOn: aCanvas
	aCanvas drawString: self compiledMethod methodClass asString 
		in: (self bounds insetBy: (Margin left: 10 top: 10 right: 2 bottom: 10)) 
		font: (LogicalFont familyName: self familyName pointSize: 11)
		color: Smalltalk ui theme textColor. 