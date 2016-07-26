accessing
labelColor
	^ labelColor ifNil: [ labelColor := self theme growlLabelColorFor: self ]