exit code handling
withReturnHandlerDo: callBlock
	^ callBlock value handleLGitReturnCode