example
example1
	"Create a thumbnail representing another Morph. The thumbnail is continously updated."
	"self example1"
	| t r |
	r := Morph new.
	r position: 100@200.
	t := ThumbnailMorph new objectToView: r viewSelector: #openInWorld.
	t openInWorld