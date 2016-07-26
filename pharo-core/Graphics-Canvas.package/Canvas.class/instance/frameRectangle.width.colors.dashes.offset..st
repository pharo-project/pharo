drawing-rectangles
frameRectangle: aRectangle width: width colors: colors dashes: dashes offset: offset
	"Draw a rectangle with the given width, colors and dash lengths.
	The offset specifies how the coordinate system is translated from the screen origin
	(infinite forms are 0@0 screen based)."

	|o s hf vf c r ds di d os l|
	width < 1 ifTrue: [^self].
	dashes size < 2
		ifTrue: [^self  frameRectangle: aRectangle width: width color: colors first].
	r := aRectangle.
	s := dashes sum * width.
	ds := dashes size.
	di := 1.
	d := (dashes at: di) * width.
	c := colors at: di.
	hf := Form extent: s @ 1 depth: 32.
	r height >= width ifTrue: [
		o := r left + offset x \\ s.
		0 to: s - 1 do: [:x |
			hf colorAt: x + o \\ s  @ 0 put: c.
			d := d - 1.
			d = 0 ifTrue: [
				di := di \\ ds + 1.
				d := (dashes at: di) * width.
				c := colors at: di]].
		os := 0.
		l := r width truncateTo: width.
		self
			fillRectangle: (r topLeft + (os@0) extent: l@width)
			color: (InfiniteForm with: hf)].
	vf := Form extent: 1 @ s depth: 32.
	r width >= width ifTrue: [
		o := r top + offset y + width + (s - (r width \\ s)) \\ s.
		0 to: s - 1 do: [:y |
			vf colorAt: 0 @ (y + o \\ s) put: c.
			d := d - 1.
			d = 0 ifTrue: [
				di := di \\ ds + 1.
				d := (dashes at: di) * width.
				c := colors at: di]].
		os := width - (r width \\ width).
		l := r height - os truncateTo: width.
		self
			fillRectangle: (r topRight + (width negated @ os) extent: width@l)
			color: (InfiniteForm with: vf)].
	r height > width ifTrue: [
		o := r right + offset x - (width * 2) + (r height \\ s) + (r width \\ s) \\ s.
		0 to: s - 1 do: [:x |
			hf colorAt: o + s -1 - x \\ s  @ 0 put: c.
			d := d - 1.
			d = 0 ifTrue: [
				di := di \\ ds + 1.
				d := (dashes at: di) * width.
				c := colors at: di]].
		os := width - (r width \\ width + (r height \\ width) \\ width).
		l := r width - os truncateTo: width.
		os := (r width - os) \\ width.
		self
			fillRectangle: (r bottomLeft + (os @ width negated) extent: l@width)
			color: (InfiniteForm with: hf)].
	r width > width ifTrue: [
		o :=  r top + offset y + (r height * 2 \\ s) + (r width * 2 \\ s) - (width * 3) \\ s.
		0 to: s - 1 do: [:y |
			vf colorAt: 0 @ (o + s -1 - y \\ s) put: c.
			d := d - 1.
			d = 0 ifTrue: [
				di := di \\ ds + 1.
				d := (dashes at: di) * width.
				c := colors at: di]].
		l := r height - (2 * width) + os.
		os := width.
		self
			fillRectangle: (r topLeft + (0@os) extent: width@l)
			color: (InfiniteForm with: vf)]