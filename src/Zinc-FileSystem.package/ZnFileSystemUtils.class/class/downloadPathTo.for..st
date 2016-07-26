paths
downloadPathTo: directoryOrFileName for: baseName
	| downloadPath |
	downloadPath := directoryOrFileName asFileReference.
	^ downloadPath isDirectory
		ifTrue: [
			baseName ifNil: [ 
				self error: 'Not enough information to construct a download path' ]. 
			(downloadPath / baseName) fullName ]
		ifFalse: [ 
			directoryOrFileName ]
	