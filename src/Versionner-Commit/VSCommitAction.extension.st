Extension { #name : #VSCommitAction }

{ #category : #'*Versionner-Commit-UI' }
VSCommitAction >> browseOn: aBrowser [
	self subclassResponsibility
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitAction >> currentString [
	^ self subclassResponsibility
]

{ #category : #'*Versionner-Commit-UI' }
VSCommitAction >> displayString [
	^ self subclassResponsibility

]

{ #category : #'*Versionner-Commit-UI' }
VSCommitAction >> icon [
	^ self subclassResponsibility
]
