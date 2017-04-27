special filters
isPackageNamed: packageName 

	^ self
		title: 'belongs to package named ', packageName
		condition: [ :e | 
			e content isCodeChange and: [ 
			e content affectedPackageName = packageName ] ]