initialize-release
list: weakFinalizationList object: anObject
	self assert: (weakFinalizationList class == WeakFinalizationList).
	list := weakFinalizationList.
	self at: 1 put: anObject.
