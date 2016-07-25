private
trimVersionInfos: versionInfos

	| trimmed count marker cv |
	self trimVersionLists ifFalse: [ ^versionInfos ].
	versionInfos size <= 10 ifTrue: [ ^versionInfos ].
	trimmed := (versionInfos copyFrom: 1 to: 5) asOrderedCollection.
	marker := count := 0.
	cv := self currentVersion.
	 6 to: versionInfos size do: [:index | | info |
		info := versionInfos at: index.
		info isStable | info isBleedingEdge | info isDevelopment | info hasValidationIssues | (cv = info version)
			ifTrue: [ 
				count - 1 >= marker ifTrue: [ trimmed add:( MBLabelInfo new name: '..') ].
				trimmed add: info.
				marker := count. ].
		count := count + 1 ].
	count - 1 >= marker ifTrue: [ trimmed add:( MBLabelInfo new name: '..') ].
	^trimmed