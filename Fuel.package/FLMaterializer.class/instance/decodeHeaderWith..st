protected
decodeHeaderWith: aDecoder
	"See FLSerializer>>encodeHeaderWith:"
	
	"todo: fix"
	"Here I cannot use #materializeFrom: again because I will end up in an infinitive loop. Therefore, I use #materializationFactory."
	^(aDecoder nextEncodedByte = 0) 
		ifTrue: [ FLHeader new ]
		ifFalse: [ (self class newDefault materializationFactory value: aDecoder) root ]