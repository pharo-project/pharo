initialize-release
registerClass: aClass atIndex: anIndex usingReadSelector: readSelector usingWriteSelector: writeSelector
	self typeMap at: aClass put: anIndex.
	self readSelectors at: anIndex put: readSelector.
	self writeSelectors at: anIndex put: writeSelector