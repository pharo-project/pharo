running
run
	
	'changelog.xml' asFileReference ensureDelete writeStreamDo: [ :stream|
		self generateOn: stream ]