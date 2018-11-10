"
I define a git_index_matched_path_cb.

https://libgit2.github.com/libgit2/#HEAD/group/callback/git_index_matched_path_cb
"
Class {
	#name : #LGitIndexMatchedPathCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitIndexMatchedPathCallback class >> fnSpec [
	^ #(int (String path, String matched_pathspec, void *payload))
]
