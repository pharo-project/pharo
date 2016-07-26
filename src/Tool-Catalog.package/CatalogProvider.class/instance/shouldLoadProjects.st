private
shouldLoadProjects
	^ projects isEmptyOrNil
		or: [ lastModified notNil
				and: [ DateAndTime now - lastModified > 24 hours ] ]