Class {
	#name : #LGitDiffStats,
	#superclass : #LGitRepositoryObject,
	#category : #'LibGit-Core-NonCoreObjects'
}

{ #category : #'libgit-calls' }
LGitDiffStats class >> diff_stats_free: handle [
	
	^ self call: #(void git_diff_stats_free #(void *handle)) options: #()
]

{ #category : #'libgit-finalization' }
LGitDiffStats class >> freeFunctionSelector [
	^ #diff_stats_free:
]

{ #category : #'libgit-calls' }
LGitDiffStats >> diff_stats_deletions: stats [
	
	^ self
		call: #(size_t git_diff_stats_deletions #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiffStats >> diff_stats_files_changed: stats [
	
	^ self
		call: #(size_t git_diff_stats_files_changed #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiffStats >> diff_stats_insertions: stats [
	
	^ self
		call: #(size_t git_diff_stats_insertions #(self))
		options: #()
]

{ #category : #'libgit-calls' }
LGitDiffStats >> diff_stats_to_buf: out stats: stats format: format width: width [
	
	^ self
		call:
			#(LGitReturnCodeEnum git_diff_stats_to_buf #(LGitBuffer out , self , LGitDiffStatsFormatTypeEnum format , size_t width))
		options: #()
]
