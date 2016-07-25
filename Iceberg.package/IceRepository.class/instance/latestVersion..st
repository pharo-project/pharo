versions handling
latestVersion: packageName
	| package |
	package := Iceberg packageNamed: packageName.
	^ (self versionsFor: package) detectMax: #versionNumber.