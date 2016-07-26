instance creation
openForVersion: developmentVersion
	| releasePanel |
	
	releasePanel := self new.
	releasePanel developmentVersion: developmentVersion.
	^ releasePanel openDialogWithSpec