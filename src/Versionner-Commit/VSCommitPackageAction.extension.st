Extension { #name : #VSCommitPackageAction }

{ #category : #'*Versionner-Commit-UI' }
VSCommitPackageAction >> browseOn: aBrowser [
	aBrowser browsePackage: self
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitPackageAction >> currentString [
	| workingCopy |
	
	workingCopy := self package workingCopy.
	^ [ workingCopy ancestry ancestorStringWithout: workingCopy packageName ]
		on: Error do: [ :e | '' ]
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitPackageAction >> displayString [ 
	^ self package name
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitPackageAction >> icon [
	^ Smalltalk ui icons iconNamed: #packageIcon
]
