"
A LGitTag represents a git tag and the associate information.

Instance Variables
	handle:		NBExternalObject

handle
	- the handle to the external data object

"
Class {
	#name : #LGitTag,
	#superclass : #LGitObject,
	#category : #'LibGit-Core-CoreObjects'
}

{ #category : #'libgit-finalization' }
LGitTag class >> freeFunctionSelector [
	^ #tag_free:
]

{ #category : #testing }
LGitTag class >> isType: aType [
	^ aType = LGitObjectTypeEnum git_obj_tag
]

{ #category : #'libgit-calls' }
LGitTag class >> tag_free: handle [
	^ self call: #(void git_tag_free #(void *handle)) options: #()
]

{ #category : #accessing }
LGitTag >> name [
	^ self tag_name
]

{ #category : #accesing }
LGitTag >> object [
	| peeled |
	peeled := LGitObject of: self repository.
	self tag_peel: peeled.
	^ peeled concreteObject
]

{ #category : #'libgit-calls' }
LGitTag >> tag_name [
	^ self call: #(String git_tag_name #(self)) options: #()
]

{ #category : #'libgit-calls' }
LGitTag >> tag_peel: result [
	^ self
		call: #(#LGitReturnCodeEnum git_tag_peel(LGitObject *result, self))
		options: #()
]
