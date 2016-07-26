layout
layoutBounds
	"Return the bounds for laying out children of the receiver"
	| inset box |
	inset := self layoutInset.
	box := self innerBounds.
	inset isZero ifTrue:[^box].
	^box insetBy: inset.