mocks
mockVersionInfo
	^ MCVersionInfo
		name: self name
		id: (self uuidForName: name)
		message: ''
		date: nil
		time: nil
		author: ''
		ancestors: #()