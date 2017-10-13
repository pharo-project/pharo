reading
doReadFrom: aReader
	| originalValue |
	originalValue := BoxedFloat64 new.
	originalValue at:1 put: (aReader readUInt32).
	originalValue at:2 put: (aReader readUInt32).
	
	"Force the conversion to SmallFloat64 or BoxedFloat64"
	value := originalValue + 0.
	
	(value literalEqual: originalValue) ifFalse:[ value := value negated]