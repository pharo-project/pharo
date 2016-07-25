testing
testReadFrom
	"self debug: #testReadFrom"
	
	
	| st rg |
	rg := Point asRingDefinition.
	st := String streamContents: [:s |
		rg storeOn: s.
		s contents].
	self assert: (Object readFrom: st) = rg. 
	
	rg := Point class asRingDefinition.
	st := String streamContents: [:s |
		rg storeOn: s.
		s contents].
	self assert: (Object readFrom: st) = rg 