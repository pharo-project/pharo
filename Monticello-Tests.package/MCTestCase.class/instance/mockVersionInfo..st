mocks
mockVersionInfo: tag 
	^ MCVersionInfo
		name: self mockVersionName, '-', tag asString
		id: UUID new
		message: self mockMessageString, '-', tag asString
		date: Date today
		time: Time now
		author: Author fullName
		ancestors: #()
