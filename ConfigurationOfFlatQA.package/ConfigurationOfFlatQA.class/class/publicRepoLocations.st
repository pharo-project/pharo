properties
publicRepoLocations

	^	(self supportedVersions collect: [ :v |
			'http://www.smalltalkhub.com/mc/Pharo/MetaRepoForPharo{1}0/main'
			format: { v } ]),
		
		#('http://www.smalltalkhub.com/mc/Pharo/Pharo60Inbox/main')