reading
doReadFrom: aReader
	value := BoxedFloat64 new.
	value at:1 put: (aReader readUInt32).
	value at:2 put: (aReader readUInt32).
	
	"Force the conversion to SmallFloat64 or BoxedFloat64"
	value := value + 0