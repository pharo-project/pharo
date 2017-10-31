some tests
testUnionEmpty
	| upper result result2 lower |
	upper := 'ABCDEFGHIJKLMNOPQRSTUVWXYZ' as: CharacterSet.
	self assert: (upper isKindOf: CharacterSet).
	
	lower := 'aeiou' as: CharacterSet.
	self assert: (lower isKindOf: CharacterSet).
	
	result := lower intersection: upper.
	self assert: (result isKindOf: CharacterSet).
	self assert: result isEmpty.
	
	result2 := upper intersection: lower.
	self assert: (result2 isKindOf: CharacterSet).
	self assert: result2 isEmpty