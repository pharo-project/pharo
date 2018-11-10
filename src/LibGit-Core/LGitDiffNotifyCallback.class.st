"
Diff notification callback function.

https://libgit2.github.com/libgit2/#HEAD/group/callback/git_diff_notify_cb

The callback will be called for each file, just before the git_delta_t gets inserted into the diff.

When the callback: - returns < 0, the diff process will be aborted. - returns > 0, the delta will not be inserted into the diff, but the diff process continues. - returns 0, the delta is inserted into the diff, and the diff process continues.


"
Class {
	#name : #LGitDiffNotifyCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitDiffNotifyCallback class >> fnSpec [
	^#(int (LGitDiff *diff_so_far, LGitDiffDelta *delta_to_add, String matched_pathspec, void *payload))
]
