Class {
	#name : #LGitPackBuilder,
	#superclass : #LGitExternalObject,
	#category : #'LibGit-Core-HelperObjects'
}

{ #category : #'native-boost' }
LGitPackBuilder class >> freeFunctionSelector [
	^ #packbuilder_free:
]

{ #category : #'libgit-calls' }
LGitPackBuilder class >> packbuilder_free: handle [
	
	^ self
		call: #(void git_packbuilder_free #(void *handle))
		options: #()
]

{ #category : #'libgit-calls' }
LGitPackBuilder >> packbuilder_foreach: pb cb: cb payload: payload [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_packbuilder_foreach #(self , LGitPackbuilderForeachCallback cb ,  void *payload))
		options: #(optMayGC optCoerceNilToNull)
]

{ #category : #'libgit-calls' }
LGitPackBuilder >> packbuilder_insert_commit: pb id: id [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_packbuilder_insert_commit #(self , LGitId * id))
		options: #(optMayGC)
]

{ #category : #'libgit-calls' }
LGitPackBuilder >> packbuilder_insert_tree: pb id: id [
	
	^ self
		call: #(LGitReturnCodeEnum git_packbuilder_insert_tree #(self , LGitId * id))
		options: #(optMayGC)
]

{ #category : #'libgit-calls' }
LGitPackBuilder >> packbuilder_new: out repo: repo [
	
	^ self
		callUnchecked:
			#(LGitReturnCodeEnum git_packbuilder_new #(LGitPackBuilder * self , LGitRepository repo))
		options: #()
]
