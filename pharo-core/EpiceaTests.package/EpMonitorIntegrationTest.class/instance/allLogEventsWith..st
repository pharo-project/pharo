running
allLogEventsWith: aClass

	^ (self allLogEntriesWith: aClass) collect: #content