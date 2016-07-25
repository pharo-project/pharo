as yet unclassified
restoreClassAComment
	Smalltalk globals
		at: #MCMockClassA
		ifPresent: [ :a | a classComment: self classAComment stamp: self classACommentStamp ]