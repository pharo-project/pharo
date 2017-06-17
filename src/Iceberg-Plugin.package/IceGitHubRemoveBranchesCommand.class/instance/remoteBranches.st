private
remoteBranches
	^ branches ifNil: [ branches := STON fromString: self requestRemoteBranches ]