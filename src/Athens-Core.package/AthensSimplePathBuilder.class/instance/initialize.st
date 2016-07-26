initialize-release
initialize
	"A new path always starts from implicit (moveTo:0@0) segment.
	If next segment is moveTo: , the point of already existing move segment will be changed,
	avoiding creating extra move segments.
	
	"
	absolute := false.
	contourStartPt := ZeroPoint.
	pathStart := lastSegment := (AthensMoveSegment new  point: ZeroPoint).