Extension { #name : #MTPackage }

{ #category : #'*Versionner-Commit' }
MTPackage >> asCommitAction [
	^ VSCommitPackageAction package: self
]

{ #category : #'*Versionner-Commit' }
MTPackage >> isDirty [
	^ self workingCopy 
		ifNotNil: [ :wc | wc modified ]
		ifNil: [ false ]
]

{ #category : #'*Versionner-Commit' }
MTPackage >> repository [
	^ self project repository
]

{ #category : #'*Versionner-Commit' }
MTPackage >> workingCopy [
	^ MCWorkingCopy allManagers 
		detect: [ :each | each packageName = self name ]
		ifNone: [ nil ]
]
