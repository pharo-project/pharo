Class {
	#name : #LGitStatusList,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-NonCoreObjects'
}

{ #category : #'libgit-finalization' }
LGitStatusList class >> freeFunctionSelector [
	^ #status_list_free:
]

{ #category : #'libgit-calls' }
LGitStatusList class >> status_list_free: handle [
	
	^ self
		call: #(void git_status_list_free #(void *handle))
		options: #()
]

{ #category : #'libgit-calls' }
LGitStatusList >> status_list_get_perfdata: out status: status [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_status_list_get_perfdata #(LGitPerfData * out , self))
		options: #()
]
