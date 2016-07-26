*Manifest-Tests
testStoreOn
	"self debug: #testStoreOn"
	
	
	| st |
	st := String streamContents: [:s |
		
		(Point>>#x) asRingDefinition storeOn: s.
		s contents].
	self assert: st = '(RGMethodDefinition className: ''Point'' selector: #x isMetaSide: false)'