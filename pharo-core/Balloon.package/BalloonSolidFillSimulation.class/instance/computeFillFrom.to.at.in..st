computing
computeFillFrom: minX to: maxX at: yValue in: form
	| bb |
	color isTransparent ifFalse:[
		bb := BitBlt toForm: form.
		bb fillColor: color.
		bb destX: 0 destY: 0 width: (maxX - minX) height: 1.
		bb combinationRule: Form over.
		bb copyBits].