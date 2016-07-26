instance-creation
from: aVersionInfo
	^ self new
		initializeWithName: aVersionInfo name
		id: aVersionInfo id
		message: aVersionInfo message
		date: aVersionInfo date
		time: aVersionInfo time
		author: aVersionInfo author
		ancestors: aVersionInfo ancestors
		stepChildren: aVersionInfo stepChildren