*Komitter
silentUniqueVersionNameIn: aRepository
	|versionName|

	counter := nil.
	[
		versionName := self nextVersionName.
		aRepository includesVersionNamed: versionName
	] whileTrue.

	^ versionName