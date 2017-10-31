tests
testWithAll

	| entries |
	entries := (OmMemoryStore withAll: #(a b c)) entries.
	self assertCollection: (entries collect: #content) hasSameElements: #(a b c).