updating
isAutoUpdating
	^autoUpdateProcess notNil and: [ autoUpdateProcess isSuspended  not ]