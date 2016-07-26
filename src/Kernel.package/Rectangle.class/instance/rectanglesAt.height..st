rectangle functions
rectanglesAt: y height: ht
	(y+ht) > self bottom ifTrue: [^ Array new].
	^ Array with: (origin x @ y corner: corner x @ (y+ht))