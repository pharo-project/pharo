comparing
= another

	^ self class = another class
		and: [ self versionName = another versionName
		and: [ self repositoryDescription = another repositoryDescription ] ]