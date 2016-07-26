examples
gtExampleAnnouncement
	<gtExample>
	^ self new
			data: #[ 1 2 3 4 ];
			timestamp: DateAndTime now;
			imageHash: SmalltalkImage current imagePath hash;
			sessionCreationTime: SmalltalkImage current session creationTime;
			imageVersion: SystemVersion current version;
			latestUpdate: SystemVersion current highestUpdate;
			yourself