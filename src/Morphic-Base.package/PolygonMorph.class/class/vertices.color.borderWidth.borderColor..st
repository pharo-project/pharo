instance creation
vertices: verts color: c borderWidth: bw borderColor: bc
	"(PolygonMorph
		vertices: {261@400. 388@519. 302@595. 	222@500.	141@583. 34@444}
		color: Color blue
		borderWidth: 3
		borderColor: Color black) openInWorld"
	^ self basicNew beStraightSegments vertices: verts color: c borderWidth: bw borderColor: bc