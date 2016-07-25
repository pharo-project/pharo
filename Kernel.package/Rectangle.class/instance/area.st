accessing
area
	"Answer the receiver's area, the product of width and height."
	| w |
	(w := self width) <= 0 ifTrue: [ ^ 0 ].
	^ w * self height max: 0