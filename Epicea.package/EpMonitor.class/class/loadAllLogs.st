accessing
loadAllLogs

	^self findLogFiles collect: [ :each | EpLog fromFile:  each]