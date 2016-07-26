protocol
openAndCommitToMonticelloWorkingCopiesFilteredBy: aFilterBlock
	| stagingArea |

	KomitStagingArea resetCurrent.
	stagingArea := KomitStagingArea currentFilteredBy: aFilterBlock.
	^ self new
		stagingArea: stagingArea;
		openAndCommitToMonticello;
		yourself