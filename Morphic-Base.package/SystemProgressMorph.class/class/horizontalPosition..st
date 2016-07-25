accessing
horizontalPosition: aFloat
	"0.0: left ... 1.0: right"
	horizontalPosition := aFloat.
	self uniqueInstance reposition.