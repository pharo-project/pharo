load primitives
loadOneAfterTheOther: aCollection merge: aBoolean
	
	(self newerVersionsIn: aCollection)
		do: [:fn | | loader |
			loader := aBoolean
				ifTrue: [ MCVersionMerger new ]
				ifFalse: [ MCVersionLoader new].
			loader addVersion: (self repository loadVersionFromFileNamed: fn).
			aBoolean
				ifTrue: [
					[loader merge] on: MCMergeResolutionRequest do: [:request |
							request autoMerge ]]
				ifFalse: [loader load]].
	

