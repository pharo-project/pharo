Extension { #name : #VSCommitProjectAction }

{ #category : #'*Versionner-Commit-UI' }
VSCommitProjectAction >> browseOn: aBrowser [
	aBrowser browseProject: self
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitProjectAction >> currentString [
	^ [ self project stableVersion ifNotNil: [  :version | version versionString  ] ]
	on: Error 
	do: [ :e | '' ]
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitProjectAction >> displayString [
	^ self project name
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitProjectAction >> icon [
	^ Smalltalk ui icons iconNamed: #config
]
