manage swipe
abortedSwipe
	[ hasAbortedSwipe := true.
	(DelayWaitTimeout forSeconds: 2) wait.
	hasAbortedSwipe := false ] fork