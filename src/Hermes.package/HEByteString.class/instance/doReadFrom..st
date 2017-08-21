reading
doReadFrom: aReader
	value := self doConvertValue:(aReader readByteString).