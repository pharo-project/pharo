*Manifest-Tests
testReadFrom
	"self debug: #testReadFrom"
	
	
	| st rg |
	rg := (Point>>#x) asRingDefinition.
	st := String streamContents: [:s |
		
		rg storeOn: s.
		s contents].
	self assert: (Object readFrom: st) = rg 