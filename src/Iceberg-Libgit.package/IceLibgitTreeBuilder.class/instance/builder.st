accessing
builder
	^ builder ifNil: [ 
		builder := LGitTreeBuilder 
			of: self repository
			fromTree: self initialTree ]
