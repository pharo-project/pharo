triggering
doInJob: aBlock
	"Evaluate the block"

	self flag: #fix. "This is a workaround so that Job considers myself as the Job's owner"

	^ [ aBlock value ] asJob
		title: '';
		run