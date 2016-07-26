running
execute: aWorkingCopy
	| version |
	version := MetacelloPlatform current newVersionForWorkingCopy: aWorkingCopy.
	self gofer repositories
		do: [ :repository | repository storeVersion: version ]