coloring
reverse
	"Change all the bits in the receiver that are white to black, and the ones 
	that are black to white."

"
Display reverse: (10@10 extent: 100@100)
"

	self fill: self boundingBox rule: Form reverse fillColor: (self quickHighLight: self depth)