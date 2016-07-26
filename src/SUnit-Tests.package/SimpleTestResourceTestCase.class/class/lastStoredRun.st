history
lastStoredRun
	^ ((Dictionary new) add: (#passed->((Set new) add: #testResourceInitRelease; add: #testResourcesCollection; yourself)); add: (#timeStamp->'22 November 2008 10:11:35 pm'); add: (#failures->((Set new))); add: (#errors->((Set new))); yourself)