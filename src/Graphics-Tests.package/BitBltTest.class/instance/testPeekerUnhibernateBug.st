tests - bugs
testPeekerUnhibernateBug
	"self run: #testPeekerUnhibernateBug"

	| bitBlt |
	bitBlt := BitBlt bitPeekerFromForm: Display.
	bitBlt destForm hibernate.
	bitBlt pixelAt: 1 @ 1