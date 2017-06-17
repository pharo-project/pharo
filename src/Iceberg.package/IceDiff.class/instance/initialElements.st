initialization
initialElements
	^ repository savedPackages collect: [ :package | self createChangeSetFor: package ]