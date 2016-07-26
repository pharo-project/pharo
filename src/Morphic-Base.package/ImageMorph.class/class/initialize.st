initialization
initialize
	"ImageMorph initialize"

	| h p d |
	DefaultForm := (Form extent: 80@40 depth: 16).
	h := DefaultForm height // 2.
	0 to: h - 1 do: [:i |
		p := (i * 2)@i.
		d := i asFloat / h asFloat.
		DefaultForm fill:
			(p corner: DefaultForm extent - p)
			fillColor: (Color r: d g: 0.5 b: 1.0 - d)].
