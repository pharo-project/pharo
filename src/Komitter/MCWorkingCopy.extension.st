Extension { #name : #MCWorkingCopy }

{ #category : #'*Komitter' }
MCWorkingCopy >> baseSnapshot [

	^ self ancestors
		ifEmpty: [ nil ]
		ifNotEmpty: [ :ancestors | self findSnapshotWithVersionInfo: ancestors first ]
]

{ #category : #'*Komitter' }
MCWorkingCopy >> collectSliceDependenciesWithMessage: messageString in: aRepository [
	
	^ self requiredPackages collect: [:aPackage | 
			MCVersionDependency
				package: aPackage
				info:  aPackage version info ]
	
]

{ #category : #'*Komitter' }
MCWorkingCopy >> remotes [

	^ self repositoryGroup remotes
]

{ #category : #'*Komitter' }
MCWorkingCopy >> silentUniqueVersionNameIn: aRepository [
	|versionName|

	counter := nil.
	[
		versionName := self nextVersionName.
		aRepository includesVersionNamed: versionName
	] whileTrue.

	^ versionName
]

{ #category : #'*Komitter' }
MCWorkingCopy >> silentlyNewSliceVersionWithName: nameString message: messageString in: aRepository [
	| info deps |
	
	info := ancestry infoWithName: nameString message: messageString.
	ancestry := MCWorkingAncestry new addAncestor: info.
	self 
		modified: true;
		modified: false.
	
	deps := self collectSliceDependenciesWithMessage: messageString in: aRepository.
	
	(self repositoryGroup includes: aRepository) 
		ifFalse: [ self repositoryGroup addRepository: aRepository ].

	^ MCVersion
		silentlyPackage: package
		info: info
		snapshot: package snapshot
		dependencies: deps
]

{ #category : #'*Komitter' }
MCWorkingCopy >> silentlyNewVersionWithName: nameString message: messageString in: aRepository [
	
	| info deps |
	info := ancestry infoWithName: nameString message: messageString.
	ancestry := MCWorkingAncestry new addAncestor: info.
	self modified: true.
	
	deps := self collectDependenciesWithMessage: messageString in: aRepository.
	
	(self repositoryGroup includes: aRepository) 
		ifFalse: [ self repositoryGroup addRepository: aRepository ].

	^ MCVersion
		silentlyPackage: package
		info: info
		snapshot: package snapshot
		dependencies: deps
]
