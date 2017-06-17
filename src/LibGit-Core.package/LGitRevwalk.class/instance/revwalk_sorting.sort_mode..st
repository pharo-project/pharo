libgit-calls
revwalk_sorting: walk sort_mode: sort_mode
	
	^ self
		call: #(FFIVoid git_revwalk_sorting #(self , FFIUInt32 sort_mode))
		options: #()