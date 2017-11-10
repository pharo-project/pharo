Extension { #name : #GTFuelPacking }

{ #category : #'*GT-Spotter-EventRecorder' }
GTFuelPacking >> migrationForGTSpotterRecorderDataAnnouncement [
	<fuelMaterializerMigration>
	^ [ :aMaterializer | aMaterializer 
				migrateClassNamed: #GTSpotterRecorderDataAnnouncement toClass: GTEventAnnouncement ]
]
