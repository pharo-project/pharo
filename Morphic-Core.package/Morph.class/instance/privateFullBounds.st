layout
privateFullBounds
	"Private. Compute the actual full bounds of the receiver"

	| box |
	submorphs isEmpty ifTrue: [^self outerBounds].
	box := self outerBounds copy.
	box := box quickMerge: (self clipSubmorphs 
						ifTrue: [self submorphBounds intersect: self clippingBounds ifNone: [ self clippingBounds ]]
						ifFalse: [self submorphBounds]).
	^box origin asIntegerPoint corner: box corner asIntegerPoint