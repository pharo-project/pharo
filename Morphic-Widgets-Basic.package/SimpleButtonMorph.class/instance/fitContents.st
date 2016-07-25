accessing
fitContents
	| aMorph aCenter |
	aCenter := self center.
	submorphs isEmpty ifTrue: [^self].
	aMorph := submorphs first.
	self extent: aMorph extent + (borderWidth + 6).
	self center: aCenter.
	aMorph position: aCenter - (aMorph extent // 2)