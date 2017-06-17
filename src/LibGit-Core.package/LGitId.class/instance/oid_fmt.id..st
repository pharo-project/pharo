libgit-calls
oid_fmt: out id:  objectId
	^self call: #(void git_oid_fmt(ExternalAddress out, LGitId * self)) options: #(  )