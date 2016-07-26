private
updateAnnouncement: aGTEventAnnouncement for: aGTEventCollector
	^ aGTEventAnnouncement
			data: (self serialize: aGTEventCollector bundle);
			category: aGTEventCollector category;
			serializedBy: self class name;
			eventRecorderVersion: self version;
			timestamp: DateAndTime now;
			imageHash: SmalltalkImage current imagePath hash;
			imageVersion: SystemVersion current version;
			latestUpdate: SystemVersion current highestUpdate;
			computerUUID: GlobalIdentifier uniqueInstance computerUUID asString;
			sessionUUID: SmalltalkImage current session id asString;
			sessionCreationTime: SmalltalkImage current session creationTime;
			yourself