fields description
fieldsDesc
	^#(	
	LGitDeltaTypeEnum status
	#FFIUInt32 flags
	"for RENAMED and COPIED, value 0-100"
	#FFIUInt16 similarity
	"number of files in this delta"
	#FFIUInt16 nfiles
	LGitDiffFile old_file
	LGitDiffFile new_file
	)