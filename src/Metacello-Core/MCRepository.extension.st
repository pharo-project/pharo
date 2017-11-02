Extension { #name : #MCRepository }

{ #category : #'*metacello-core' }
MCRepository >> flushForScriptGet [
    "noop"
]

{ #category : #'*metacello-core' }
MCRepository >> repositoryBranchName [
  "extract a branch name from the repository ... if possible"

  ^ ''
]

{ #category : #'*metacello-core' }
MCRepository >> repositoryDescription [
  "return a description that includes a repositoryVersionString and repositoryBranchName if present"

  ^ self description
]

{ #category : #'*metacello-core' }
MCRepository >> repositoryVersionString [
  "extract a version string from the repository ... if possible"

  ^ ''
]
