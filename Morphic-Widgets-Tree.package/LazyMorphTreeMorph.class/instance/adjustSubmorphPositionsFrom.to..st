initialize - release
adjustSubmorphPositionsFrom: start to: stop
	"Fixed to not require setting item widths to 9999."
	
	| subs p |
	subs := self allNodeMorphs.
	p := (subs at: start) position.
	start to: subs size do: [:idx | | each h |
		each := subs at: idx.
		h := each height.
		each bounds: (p extent: each width@h).
		p := p + (0@h)].
