building
setMCVersionInfoWithNumber: aNumber
	| packageName |
	packageName := packageDirectory copyUpToLast: $..
	info := MCGitFileTreeVersionInfo
		name: packageName , '-' , author , '.' , aNumber printString
		id: (self class uuidFromGitSHA: commitID package: packageName)
		message: message
		date: timeStamp asDate
		time: timeStamp asTime
		author: author
		ancestors: (parents collect: [ :parent | parent info ])
		repository: repository