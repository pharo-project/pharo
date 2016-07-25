protocol
openAndCommitToMonticello

	KomitStagingArea resetCurrent.
	^ self new
		stagingArea: KomitStagingArea current;
		openAndCommitToMonticello;
		yourself