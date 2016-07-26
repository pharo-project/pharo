initialization
initialize
	"We do not do a super initialize since it is not strickly necessary and more importantly MessageTally must be instantiated quickly"
	"super initialize."
	
	maxClassNameSize := self class defaultMaxClassNameSize.
	maxClassPlusSelectorSize := self class defaultMaxClassPlusSelectorSize.
	maxTabs := self class defaultMaxTabs.
	reportOtherProcesses := true.
	time := 0.
	tally := 0.