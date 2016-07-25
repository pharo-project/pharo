accessing
versionStatus

	versionStatus == nil ifTrue: [ versionStatus := self computeVersionStatus].
	^versionStatus