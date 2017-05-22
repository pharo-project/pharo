libgit-calls
libgit2_features
	"Query compile time options for libgit2."
	
	^self ffiCall: #(int git_libgit2_features())