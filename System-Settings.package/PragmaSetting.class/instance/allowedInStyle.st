accessing
allowedInStyle
	^ allowedInStyle ifNil: [allowedInStyle := self hasValue]