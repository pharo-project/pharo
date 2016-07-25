testing - geometry
testExtent
	"
	self debug: #testExtent
	"
	| m1 m2 v1 v2 v3 b1 b2 |
	m1 := Morph new.
	m2 := Morph new.
	
	v1 := 100.000001.
	v2 := 100.000001000001.
	v3 := 100.000001000002.

	m1 extent: v1@v1.
	b1 := m1 bounds.
	
	m2 extent: v2@v3.
	b2 := m2 bounds.
	
	self assert: (b2 = b1).
