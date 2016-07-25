asserting
assertContentsOf: strm match: expected 
	| actual |
	actual := strm contents.
	self assert: actual size = expected size.
	actual with: expected do: [:a :e | self assert: a = e]