private
nextCompletion
	currentCompletionIndex := (currentCompletionIndex + 1 \\ self searchList size) + 1.
	^ self currentTabCompletion