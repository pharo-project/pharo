Extension { #name : #MCLazyVersionInfo }

{ #category : #'*monticellogui' }
MCLazyVersionInfo >> loadAncestorsAndStepChildren [
	"This code scans all the repositories to find the exact package version and reload the relevant info."

	"Transient failures, such as the lack of network connectivity, will return nothing and no errors from this code."

	"Cache loaded version info, but try not to add a nil at that space? Can't do that with LRUCache>>#at:ifAbsentPut:"

	((MCTool mcVersionCache includesKey: id)
		ifTrue: [ MCTool mcVersionCache at: id ]
		ifFalse:
			[ (MCRepositoryGroup default
				in: [ :r | r versionWithInfo: self ifNone: [  ] ])
				ifNil: [  ]
				ifNotNil:
					[ :aVersion | 
					MCTool mcVersionCache at: id put: aVersion.
					aVersion ] ])
		ifNotNil:
			[ :aVersion | 
			ancestors := WeakArray withAll: aVersion info ancestors.
			stepChildren := WeakArray withAll: aVersion info stepChildren ]
]
