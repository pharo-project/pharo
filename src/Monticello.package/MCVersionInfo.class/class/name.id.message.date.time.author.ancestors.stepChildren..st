instance-creation
name: vName id: id message: message date: date time: time author: author ancestors: ancestors stepChildren: stepChildren
	^ self new
		initializeWithName: vName
		id: id
		message: message
		date: date
		time: time
		author: author
		ancestors: ancestors
		stepChildren: stepChildren