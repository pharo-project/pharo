as yet unclassified
createHighlights
	"Create and store the src and dst highlights."

	|s d|
	s := OrderedCollection new.
	d := OrderedCollection new.
	s add: (self newHighlight
			color: self src color;
			borderWidth: self borderWidth;
			bounds: (0@self src range first corner: 0@(self src range last + 1));
			borderSides: #(top left bottom)).
	d add: (self newHighlight
			color: self dst color;
			borderWidth: self borderWidth;
			bounds: (0@self dst range first corner: 0@(self dst range last + 1));
			borderSides: #(top right bottom)).
	self src highlights: s.
	self dst highlights: d