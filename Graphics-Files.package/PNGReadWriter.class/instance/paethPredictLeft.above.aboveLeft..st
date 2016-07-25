filtering
paethPredictLeft: a above: b aboveLeft: c 
	"Predicts the value of a pixel based on nearby pixels, based on
Paeth (GG II, 1991)"
	| pa pb pc |
	pa := b > c 
		ifTrue: [ b - c ]
		ifFalse: [ c - b ].
	pb := a > c 
		ifTrue: [ a - c ]
		ifFalse: [ c - a ].
	pc := a + b - c - c.
	pc < 0 ifTrue: [ pc := pc * -1 ].
	(pa <= pb and: [ pa <= pc ]) ifTrue: [ ^ a ].
	pb <= pc ifTrue: [ ^ b ].
	^ c