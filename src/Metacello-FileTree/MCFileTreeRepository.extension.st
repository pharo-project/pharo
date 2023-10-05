Extension { #name : 'MCFileTreeRepository' }

{ #category : '*Metacello-FileTree' }
MCFileTreeRepository >> metacelloProjectClassFor: aScriptEngine [
    aScriptEngine versionString isEmptyOrNil
        ifFalse: [ 
            "If we have a version in the script, then a BaselineOf is not being referenced...use a MetacelloMCProject
	    	 see https://github.com/dalehenrich/metacello-work/issues/7"
            ^ super metacelloProjectClassFor: aScriptEngine ].
    ^ MetacelloMCBaselineProject
]

{ #category : '*Metacello-FileTree' }
MCFileTreeRepository >> repositoryBranchName [
  "extract a branch name from the repository ... if possible"

  (MetacelloPlatform current gitBranchNameFor: self directory fullName)
    ifNotNil: [ :sha | ^ sha ].
  ^ super repositoryBranchName
]

{ #category : '*Metacello-FileTree' }
MCFileTreeRepository >> repositoryDescription [
  ^ self description , ' [' , self repositoryVersionString , ':'
    , self repositoryBranchName , ']'
]

{ #category : '*Metacello-FileTree' }
MCFileTreeRepository >> repositoryVersionString [
  "extract a version string from the repository ... if possible"

  (MetacelloPlatform current gitCommitShaFor: self directory fullName)
    ifNotNil: [ :sha | ^ sha ].
  ^ super repositoryVersionString
]

{ #category : '*Metacello-FileTree' }
MCFileTreeRepository >> resetDirectory [
	directory := nil
]
