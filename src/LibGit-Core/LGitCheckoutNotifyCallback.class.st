Class {
	#name : #LGitCheckoutNotifyCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #accessing }
LGitCheckoutNotifyCallback class >> fnSpec [
	^#(int (LGitCheckoutNotifyTypeEnum why, String path, LGitDiffFile *baseline, LGitDiffFile *target, LGitDiffFile *workdir, void *payload))
]
