converting
visitCloseSegment: closeSegment
	
	"see if its alredy closed"
	contours last last = contours last first ifFalse: [
		
		contours last 
			add: (contours last last);
			add: closeSegment endPoint;
			add: closeSegment endPoint
	]