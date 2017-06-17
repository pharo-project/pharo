libgit-calls
treebuilder_clear: bld
	
	^ self call: #(FFIVoid git_treebuilder_clear #(self)) options: #()