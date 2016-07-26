accessing
readOnly
	^readOnly ifNil: [ readOnly := self defaultReadOnly ]