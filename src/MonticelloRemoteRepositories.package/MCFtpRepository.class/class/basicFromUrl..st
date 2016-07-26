instance creation
basicFromUrl: aZnUrl
	^ self
		host: (aZnUrl hasPort ifTrue: [ aZnUrl host, ':', aZnUrl port asString ] ifFalse: [ aZnUrl host ])
		directory: aZnUrl path "MCFtpRepository assumes NO prefixed / in the path"
		user: (aZnUrl username ifNil: [ '' ])
		password: (aZnUrl password ifNil: [ '' ])