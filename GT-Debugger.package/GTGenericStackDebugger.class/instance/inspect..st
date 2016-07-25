actions
inspect: anObject

	"Use silentValue: instead of value: as just using value: causes the current inspector to refresh the content of it current presentations without rebuiling them (the dynamic presentation contained by the inspector does not rebuild its content)"
	((self browser paneNamed: #inspector) port: #entity) silentValue: anObject.
	
	(self browser paneNamed: #inspector) update.
	