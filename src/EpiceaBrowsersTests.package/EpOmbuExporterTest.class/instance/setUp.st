running
setUp 
	super setUp.
	inputLog1 := EpLogTestResource current oneTriggerWithFourChildrenLog.
	outputLog := EpLog newWithStore: OmFileStore new.
	inputLog2 := EpLogTestResource current oneTriggerWithOneChildLog.
