instance creation
withAll: aCollection

	^ self new
		in: [:store | 
			aCollection 
				collect: [:each | OmEntry content: each ]
				thenDo: [:entry | store newEntry: entry ] ];
		yourself