Class {
	#name : #LGitPathSpecMatchList,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-NonCoreObjects'
}

{ #category : #'libgit-finalization' }
LGitPathSpecMatchList class >> freeFunctionSelector [
	^ #pathspec_match_list_free:
]

{ #category : #'libgit-calls' }
LGitPathSpecMatchList class >> pathspec_match_list_free: handle [
	
	^ self
		call: #(void git_pathspec_match_list_free #(void *handle))
		options: #()
]

{ #category : #'libgit-calls' }
LGitPathSpecMatchList >> pathspec_match_diff: out diff: diff flags: flags ps: ps [
	
	^ self
		callUnchecked:
			#(int git_pathspec_match_diff #(LGitPathSpecMatchList * self , LGitDiff diff , LGitPathSpecFlagTypeEnum flags , LGitPathSpec ps))
		options: #()
]

{ #category : #'libgit-calls' }
LGitPathSpecMatchList >> pathspec_match_list_diff_entry: m pos: pos [
	
	^ self
		call:
			#(LGitDiffDelta git_pathspec_match_list_diff_entry #(LGitPathSpecMatchList m , size_t pos))
		options: #()
]
