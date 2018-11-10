Class {
	#name : #LGitSimilarityCallback,
	#superclass : #LGitCallback,
	#category : #'LibGit-Core-FFI-Callbacks'
}

{ #category : #'callback spec' }
LGitSimilarityCallback class >> fnSpec [
	^ #(int (int *score, void *siga, void *sigb, void *payload))
]
