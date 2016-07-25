actions
adjustSubmorphPositions
	"Fixed to not require setting item widths to 9999."
	
	| p |
	p := 0@0.
	scroller submorphsDo: [ :each | | h |
		h := each height.
		each privateBounds: (p extent: each width@h).
		p := p + (0@h)
	].
	self 
		changed;
		layoutChanged;
		setScrollDeltas.
