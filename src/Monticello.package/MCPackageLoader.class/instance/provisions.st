private
provisions
	^ provisions ifNil: [provisions := Set withAll: Smalltalk globals keys]