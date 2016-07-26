accessing
displayFormat
	^ displayFormat ifNil: [displayFormat := self glamourPresentation format]