libgit-calls
giterr_clear
	^ self call: #(void giterr_clear()) options: #(  )