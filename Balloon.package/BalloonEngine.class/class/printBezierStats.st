accessing
printBezierStats
	"BalloonEngine printBezierStats"
	"BalloonEngine resetBezierStats"
	Transcript 
		cr; nextPutAll:'Bezier statistics:';
		crtab; print: (BezierStats at: 1); tab; nextPutAll:' non-monoton curves splitted';
		crtab; print: (BezierStats at: 2); tab; nextPutAll:' curves splitted for numerical accuracy';
		crtab; print: (BezierStats at: 3); tab; nextPutAll:' curves splitted to avoid integer overflow';
		crtab; print: (BezierStats at: 4); tab; nextPutAll:' curves internally converted to lines';
	endEntry.