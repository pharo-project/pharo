private
reposition
	"Calculate the proper position"
	
	| xPos yPos margin |
	margin := 20.
	
	xPos := Display boundingBox width - self fullBounds width - (2*margin).
	xPos := xPos * self class horizontalPosition + margin.
	
	yPos := Display boundingBox height - self fullBounds height - (2*margin).
	yPos := yPos * self class verticalPosition + margin.
	
	self align: self fullBounds topLeft with: Display boundingBox topLeft + (xPos @ yPos).
	
	self refresh.