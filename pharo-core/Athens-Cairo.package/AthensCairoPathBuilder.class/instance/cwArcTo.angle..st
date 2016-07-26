path commands
cwArcTo: newEndPoint angle: angle
	" Add a clockwise arc segment, starting from current path endpoint and
	ending at andPt. Angle should be specified in radians 
	"
	^ self arcTo: newEndPoint angle: angle cw: true