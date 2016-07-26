initialization
reset

	origin := 0@0.							"origin of the top-left corner of this cavas"
	clipRect := (0@0 corner: 10000@10000).		"default clipping rectangle"
	self shadowColor: nil.