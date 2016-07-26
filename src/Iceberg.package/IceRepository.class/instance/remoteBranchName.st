querying
remoteBranchName
	^ (self command: ('config branch.{1}.merge' format: { self branch })) trim 
		ifEmpty: nil
		ifNotEmpty: [ :output | output copyAfterLast: $/ ]
