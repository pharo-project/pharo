private
shouldLoadProjects
	^ session ~= Smalltalk session 
		or: [ lastModified notNil and: [ DateAndTime now - lastModified > 24 hours ] ]