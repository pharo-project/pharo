initialize-release
initialize
	"Initialize the default stop conditions."
	stops := Array new: 258.
	self endOfRun: #endOfRun.
	self crossedX: #crossedX.