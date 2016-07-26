private
processBuffer: aByteArray
	"Process given 64-byte buffer, accumulating the results in totalA through totalE."

	| a b c d e f g h w |
	totals := nil.

	"initialize registers a through e from the current totals" 
	a := totalA copy.
	b := totalB copy.
	c := totalC copy.
	d := totalD copy.
	e := totalE copy.
	f := totalF copy.
	g := totalG copy.
	h := totalH copy.

	"expand and process the buffer"
	w := self expandedBlock: aByteArray.
	1 to: 64 do: [:i | | s0 maj t0 s1 ch t1 |
		s0 := (a copy leftRotateBy: -2).
		s0 bitXor: (a copy leftRotateBy: -13);
			bitXor: (a copy leftRotateBy: -22).
		maj := (a copy bitAnd: b).
		maj bitOr: (b copy bitAnd: c);
			bitOr: (c copy bitAnd: a).
		t0 := s0 copy += maj.
		s1 := (e copy leftRotateBy: -6).
		s1 bitXor: (e copy leftRotateBy: -11);
			bitXor: (e copy leftRotateBy: -25).
		ch := (e copy bitAnd: f).
		ch bitOr: ((e copy bitInvert) bitAnd: g).
		t1 := h copy += s1 += ch += (ThirtyTwoBitRegister new load: (self class roundConstants at: i)) += (w at: i) copy.
		h := g.
		g := f.
		f := e.
		e := d copy += t1.
		d := c.
		c := b.
		b := a.
		a := t0 += t1.
	"Transcript cr; show: i asString, String tab, a asString, String tab, b asString, String tab, c asString, String tab, d asString, String tab, e asString, String tab, f asString, String tab, g asString, String tab, h asString, String tab."
	].

	"add a through e into total accumulators"
	totalA += a.
	totalB += b.
	totalC += c.
	totalD += d.
	totalE += e.
	totalF += f.
	totalG += g.
	totalH += h.
