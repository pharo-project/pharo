running
setUp 
	super setUp.
	
	logBuilder1 := EpTestLogBuilder new.
	logBuilder2 := EpTestLogBuilder new.	
	inputLog1 := logBuilder1 oneTriggerWithFourChildrenLog.
	inputLog2 := logBuilder2 oneTriggerWithOneChildLog.
	outputLog := EpLog newWithStore: OmFileStore new.
