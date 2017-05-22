accessing
baseRemote
	"This is the 'base' remote (the place where the repository is forked). This is used to 
	 suggest fork PR destination (base)"
	^ baseRemote ifNil: [ baseRemote := self obtainBaseRemote ]