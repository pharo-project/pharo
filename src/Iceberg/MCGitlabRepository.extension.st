Extension { #name : #MCGitlabRepository }

{ #category : #'*Iceberg' }
MCGitlabRepository >> httpsUrl [
	^ 'https://<1s>/<2s>.git' expandMacrosWith: self hostname with: projectPath
]

{ #category : #'*Iceberg' }
MCGitlabRepository >> scpUrl [
	^ 'git@<1s>:<2s>.git' expandMacrosWith: self hostname with: projectPath
]
