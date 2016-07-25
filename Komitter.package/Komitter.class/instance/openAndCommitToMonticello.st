opening
openAndCommitToMonticello

	self openAndDo: [ self lastCommit ifNotNil: [ self lastCommit commitToMonticello ]]