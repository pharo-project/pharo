moving changes
expungeEmptyClassChangeEntries

	changeRecords keysAndValuesRemove:
		[:className :classRecord | classRecord hasNoChanges]