libgit-calls
diff_patch_free: handle
	
	^ self call: #(void git_patch_free #(void *handle)) options: #()