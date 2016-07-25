*GT-SpotterExtensions-Core-private
allAncestorsTo: aCollection
	self ancestors do: [ :versionInfo | 
		" do not add/process ancestry-branches which have already been added/processed "
		(aCollection includes: versionInfo) ifFalse: [ 
			aCollection add: versionInfo.
			versionInfo allAncestorsTo: aCollection ] ].
	^ aCollection