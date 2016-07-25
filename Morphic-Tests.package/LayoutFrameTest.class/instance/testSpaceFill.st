tests
testSpaceFill
	| lf rectangle |
	lf := LayoutFrame identity.
	rectangle := lf layout: nil in: (50@10 corner: 150@70).
	self assert: (50@10 corner: 150@70) = rectangle