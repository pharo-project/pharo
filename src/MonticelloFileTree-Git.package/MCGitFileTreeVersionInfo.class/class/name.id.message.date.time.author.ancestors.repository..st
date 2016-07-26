instance-creation
name: vName id: id message: message date: date time: time author: author ancestors: ancestors repository: aRepo
	^ (self
		name: vName
		id: id
		message: message
		date: date
		time: time
		author: author
		ancestors: ancestors
		stepChildren: #())
		repo: aRepo;
		yourself