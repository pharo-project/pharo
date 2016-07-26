accessing
doProfileStats: aBool
	"Note: On Macintosh systems turning on profiling can significantly
	degrade the performance of Balloon since we're using the high
	accuracy timer for measuring."
	"BalloonEngine doProfileStats: true"
	"BalloonEngine doProfileStats: false"
	<primitive: 'primitiveDoProfileStats' module: 'B2DPlugin'>
	^false