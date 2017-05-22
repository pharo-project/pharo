enumerating
filesDo: filesBlockOrNil binaryFilesDo: binaryBlockOrNil hunksDo: hunksBlockOrNil linesDo: linesBlockOrNil
	^ self
		diff_foreach: self
		file_cb: (filesBlockOrNil 
			ifNotNil: [ LGitDiffFileCallback on: filesBlockOrNil ]
			ifNil: [ LGitCallback null ])
		binary_cb: (binaryBlockOrNil 
			ifNotNil: [ 	LGitDiffBinaryCallback on: binaryBlockOrNil ]
			ifNil: [ LGitCallback null ])
		hunk_cb: (hunksBlockOrNil 
			ifNotNil: [ 	LGitDiffHunkCallback on: hunksBlockOrNil ]
			ifNil: [ LGitCallback null ])
		line_cb: (linesBlockOrNil 
			ifNotNil: [ 	LGitDiffLineCallback on: linesBlockOrNil ]
			ifNil: [ LGitCallback null ])
		payload: ExternalAddress null