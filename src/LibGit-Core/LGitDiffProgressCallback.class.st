"
Diff progress callback.

https://libgit2.github.com/libgit2/#HEAD/group/callback/git_diff_progress_cb

Called before each file comparison.
"
Class {
	#name : #LGitDiffProgressCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitDiffProgressCallback class >> fnSpec [
	^ #(int (LGitDiff *diff_so_far, String old_path, String new_path, void *payload))
]

{ #category : #'callback spec' }
LGitDiffProgressCallback >> fnSpec [
	^ #(int (LGitDiff *diff_so_far, String old_path, String new_path, void *payload))
]
