helpers
compactClassIndexForClassNamed: aName
	
	^ #(
	CompiledMethod
	Slot
	Array
	LargeNegativeInteger
	LargePositiveInteger
	Float
	Protocol
	Association
	Point
	Rectangle
	ByteString
	BlockClosure
	nil
	MethodContext
	nil
	Bitmap
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil
	nil) indexOf: aName