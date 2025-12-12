Extension { #name : 'MCFileTreeRepository' }

{ #category : '*Metacello-FileTree' }
MCFileTreeRepository >> repositoryDescription [
  ^ self description , ' [' , self repositoryVersionString , ':'
    , self repositoryBranchName , ']'
]

{ #category : '*Metacello-FileTree' }
MCFileTreeRepository >> resetDirectory [
	directory := nil
]
