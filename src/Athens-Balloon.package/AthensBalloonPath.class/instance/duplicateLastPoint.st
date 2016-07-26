private
duplicateLastPoint

	contours last size < 2 ifFalse: [
		"duplicate last contour point"
		contours last add: (contours last last).
		]. 
