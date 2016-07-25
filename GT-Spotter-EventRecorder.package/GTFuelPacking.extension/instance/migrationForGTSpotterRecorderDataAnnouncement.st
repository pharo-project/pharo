*GT-Spotter-EventRecorder
migrationForGTSpotterRecorderDataAnnouncement
	<fuelMaterializerMigration>
	^ [ :aMaterializer | aMaterializer 
				migrateClassNamed: #GTSpotterRecorderDataAnnouncement toClass: GTEventAnnouncement ]