accessing
currentPlatform: locale during: aBlock 
	"Alter current language platform during a block"
	| backupPlatform |
	backupPlatform := self currentPlatform.
	[self currentPlatform: locale.
	aBlock value]
		ensure: [self currentPlatform: backupPlatform]