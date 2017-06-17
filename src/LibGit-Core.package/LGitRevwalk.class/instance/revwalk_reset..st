libgit-calls
revwalk_reset: revwalk
	
	^ self call: #(FFIVoid git_revwalk_reset #(self)) options: #()