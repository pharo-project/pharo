libgit-calls
oid_pathfmt: out id: id 
	^ self call: #(void git_oid_pathfmt(ExternalAddress out, LGitId * self)) options: #(  )