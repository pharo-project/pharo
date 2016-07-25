examples
example3
	"self example3"
	| poly | 
	poly := PolygonMorph
				vertices: {261@400. 388@519. 302@595. 	222@500.	141@583. 34@444}
				color: Color blue
				borderWidth: 3
				borderColor: Color black.
	poly makeOpen.
	poly openInWorld