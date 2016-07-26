balloon help
removePendingBalloonFor: aMorph
	"Get rid of pending balloon help."
	self removeAlarm: #spawnBalloonFor:.
	self deleteBalloonTarget: aMorph.