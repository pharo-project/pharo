accessing
fieldValueString
	^ self fieldValue
		ifNil: [ String empty ]
		ifNotNil: [ :value | value asString ]