triggering
doInJob: aBlock
	"Evaluate the block. This is a workaround to make Job consider me as the Job's owner."

	^ [ aBlock value ] asJob
		title: '';
		run