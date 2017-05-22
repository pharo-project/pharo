libgit-calls
giterr_set_str: string
	"GIT_EUSER = -7"
	^ self call: #(void giterr_set_str(-7, String string))