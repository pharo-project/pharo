class tags
actualClassTags
	
	(classTags size = 1 and: [classTags anyOne isRoot]) ifTrue: [ ^#() ].
	
	^classTags