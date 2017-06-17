operations
revparse: aString
	"See git-rev-parse documentation.

	Examples:
		repository revparse: 'HEAD' 	# last commit
		repository revparse: 'HEAD~5'	# 5 commits back from last commit
		repository revparse: 'develop'	# last commit from develop branch
	"

	| object |
	self assert: aString notNil.
	self assert: aString trimBoth notEmpty.
	object := LGitObject of: self.
	self withReturnHandlerDo: [ 
		self
			revparse_single: object
			repo: self
			spec: aString ].
	^ object concreteObject	
