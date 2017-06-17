initialization
analyseChanges
	| newChanges |

	newChanges := self basicChanges.
	newChanges ifEmpty: [ 
		(self package isLoaded 
			and: [ self package isModified
			and: [(self parent myVersion isKindOf: IceWorkingCopy) 
			and: [ self parent theirVersion isKindOf: IceLoadedCode ]]])
			ifTrue: [ self package workingCopy modified: false ]].
	
	changes := OrderedCollection new.
	newChanges do: [ :change | 
		change definition 
			addAnalysisToPackageChangeSet: self
			change: change ]
