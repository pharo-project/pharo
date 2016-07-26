balloon help
triggerBalloonFor: aMorph after: timeOut
	"Trigger balloon help after the given time out for some morph"
	self addAlarm: #spawnBalloonFor: with: aMorph after: timeOut.