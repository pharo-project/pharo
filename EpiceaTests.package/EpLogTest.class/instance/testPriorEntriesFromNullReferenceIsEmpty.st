tests
testPriorEntriesFromNullReferenceIsEmpty
	| priorEntries |
	priorEntries := log priorEntriesFrom: log nullReference.
	self assert: priorEntries isEmpty