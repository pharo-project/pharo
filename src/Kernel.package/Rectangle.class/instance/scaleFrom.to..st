transforming
scaleFrom: rect1 to: rect2
	"Produce a rectangle stretched according to the stretch from rect1 to rect2"
	^ (origin scaleFrom: rect1 to: rect2)
		corner: (corner scaleFrom: rect1 to: rect2)