initialize
openCenteredInWorld
	"Open me in the center of current world"
	self 
		fullBounds;
		openInWorld;
		center: self world clearArea center