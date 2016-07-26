protocol
extraSpaceForActions
	| space offsets |
	
	space := 0.
	offsets := 1.
	
	self hasMenu ifTrue: [ 
		offsets := offsets +1.
		space := space + self menuIcon width ].
	
	self actions do: [ :each | 
		space := space + each icon width.
		offsets := offsets +1 ].
	
	^ space + (offsets * self actionOffset)