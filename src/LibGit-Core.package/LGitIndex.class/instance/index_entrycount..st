libgit-calls
index_entrycount: index
	
	^ self call: #(FFIInt32 git_index_entrycount #(self)) options: #()