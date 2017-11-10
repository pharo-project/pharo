Extension { #name : #MTProject }

{ #category : #'*Versionner-Commit' }
MTProject >> asCommitAction [
	^ VSCommitProjectAction project: self
]

{ #category : #'*Versionner-Commit' }
MTProject >> stableVersion [
	^ self configurationClass project version: #stable
]

{ #category : #'*Versionner-Commit' }
MTProject >> workingCopy [
	| packageName |
	packageName := self configurationClass package name.
	^ MCWorkingCopy allManagers detect: [ :each | each packageName = packageName ]
]
