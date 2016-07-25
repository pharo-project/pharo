converting
convert: aPath
	"pipe the path segments to destination, converting
	segments on the way"
	
	contourStartPt := endPoint := 0@0.
	
	aPath sendCommandsTo: self.
	
	
	