"
I'm a compiled pathspec (used to match)
See https://libgit2.github.com/libgit2/#HEAD/type/git_pathspec
"
Class {
	#name : #LGitPathSpec,
	#superclass : #LGitExternalObject,
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #'libgit-calls' }
LGitPathSpec class >> pathspec_free: handle [
	^ self call: #(void git_pathspec_free (void *handle)) options: #()
]

{ #category : #'instance creation' }
LGitPathSpec class >> withAll: aCollection [
	^ self basicNew 
		initializeWithAll: aCollection;
		yourself
]

{ #category : #private }
LGitPathSpec >> createNewWithAll: aCollection [
	| strings |
	strings := LGitStringArray withAll: aCollection.
	self withReturnHandlerDo: [
		self
			pathspec_new: self
			with: strings ].
	self autoRelease
]

{ #category : #initialization }
LGitPathSpec >> initializeWithAll: aCollection [
	self initialize.
	self createNewWithAll: aCollection
]

{ #category : #'libgit-calls' }
LGitPathSpec >> pathspec_new: out with: pathspec [
	^ self 
		callUnchecked: #(int git_pathspec_new(LGitPathSpec *self, LGitStringArray *pathspec))
		options: #()
]
