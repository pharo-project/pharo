computing
subdivideToBeLine
	"Not a true subdivision.
	Just return a line representing the receiver and fake me to be of zero height"
	| line |
	line := BalloonLineSimulation new.
	line start: start.
	line end: end.
	"Make me invalid"
	end := start.
	via := start.
	 ^line