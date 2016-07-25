cleaning
deleteTempFileOf: aStream
	"Deletes the temp file associated to aStream"
	aStream ifNotNil: [ 
		aStream ossIsPipe ifFalse: [ aStream fullName asFileReference ensureDelete ] 
	]
	

