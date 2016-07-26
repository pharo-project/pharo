private
currentTabCompletion
	self searchList ifEmpty: [ ^ ''].
	
	currentCompletionIndex > self searchList
		ifTrue: [ currentCompletionIndex := 1].
	
	^ self searchList at: currentCompletionIndex