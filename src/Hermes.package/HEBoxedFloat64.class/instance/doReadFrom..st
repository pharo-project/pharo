reading
doReadFrom: aReader
	value := BoxedFloat64 new.
	value at:1 put: (aReader readUInt).
	value at:2 put: (aReader readUInt).	