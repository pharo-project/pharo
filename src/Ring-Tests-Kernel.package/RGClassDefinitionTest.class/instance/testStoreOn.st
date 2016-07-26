testing
testStoreOn
	"self debug: #testStoreOn"
	
	
	| st |
	st := String streamContents: [:s |
		
		(Point) asRingDefinition storeOn: s.
		s contents].
	self assert: st = '(RGClassDefinition named: #Point)'.
	
	st := String streamContents: [:s |
		
		(Point class) asRingDefinition storeOn: s.
		s contents].
	self assert: st = '((RGMetaclassDefinition named: #''Point class'') baseClass:(RGClassDefinition named: #Point))'