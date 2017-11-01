Extension { #name : #MCFileTreeRepository }

{ #category : #'*metacello-filetree' }
MCFileTreeRepository >> metacelloProjectClassFor: aScriptEngine [
    aScriptEngine versionString isEmptyOrNil
        ifFalse: [ 
            "If we have a version in the script, then a BaselineOf is not being referenced...use a MetacelloMCProject
	    	 see https://github.com/dalehenrich/metacello-work/issues/7"
            ^ super metacelloProjectClassFor: aScriptEngine ].
    ^ MetacelloMCBaselineProject
]

{ #category : #'*metacello-filetree' }
MCFileTreeRepository >> repositoryBranchName [
  "extract a branch name from the repository ... if possible"

  (MetacelloPlatform current gitBranchNameFor: self directory fullName)
    ifNotNil: [ :sha | ^ sha ].
  ^ super repositoryBranchName
]

{ #category : #'*metacello-filetree' }
MCFileTreeRepository >> repositoryDescription [
  ^ self description , ' [' , self repositoryVersionString , ':'
    , self repositoryBranchName , ']'
]

{ #category : #'*metacello-filetree' }
MCFileTreeRepository >> repositoryVersionString [
  "extract a version string from the repository ... if possible"

  (MetacelloPlatform current gitCommitShaFor: self directory fullName)
    ifNotNil: [ :sha | ^ sha ].
  ^ super repositoryVersionString
]
