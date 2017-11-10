Extension { #name : #MCWorkingCopy }

{ #category : #'*GT-SpotterExtensions-Core' }
MCWorkingCopy >> gtDisplayOn: stream [
	ancestry ancestors isEmpty 
		ifTrue: [ 
			super gtDisplayOn: stream. stream nextPutAll: ' (unsaved)' ]
		ifFalse: [ 
			ancestry ancestors first gtDisplayOn: stream.
			self needsSaving ifTrue: [ stream nextPutAll: ' (modified)' ] ]
		
]

{ #category : #'*GT-SpotterExtensions-Core' }
MCWorkingCopy >> spotterAllRepositoriesFor: aStep [
	<spotterOrder: 20>
	aStep listProcessor
		title: 'All other repositories';
		allCandidates: [ MCRepositoryGroup default repositories \ self repositoryGroup repositories ];
		itemName: [ :item | item description ];
		actLogic: [ :item :step | 
			step exit.
			self spotterCommit: self in: item ];
		filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
MCWorkingCopy >> spotterAncestorsFor: aStep [
	<spotterOrder: 2>
	self flag: #maybeRewriteForDirectStreaming.
	aStep listProcessor
		title: 'Ancestors';
		allCandidates: [ self allAncestors ];
		filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
MCWorkingCopy >> spotterCommit: workingCopy in: aRepository [
	| newVersion |

	newVersion := workingCopy newVersionIn: aRepository.
	newVersion ifNil: [ ^ self ].
	Cursor wait showWhile: [
		[ 	aRepository storeVersion: newVersion.
			aRepository storeDependencies: newVersion ] 
		ensure: [ (MCVersionInspector new version: newVersion) show ]]
]

{ #category : #'*GT-SpotterExtensions-Core' }
MCWorkingCopy >> spotterMonticelloPackageFor: aStep [
	<spotterOrder: 21>
	aStep listProcessor
		title: 'Monticello Package';
		allCandidates: [ Array with: self package ];
		itemIcon: [ Smalltalk ui icons iconNamed: #monticelloPackageIcon ];
		filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
MCWorkingCopy >> spotterPackageFor: aStep [
	<spotterOrder: 21>
	self flag: #maybeRewriteForDirectStreaming.
	aStep listProcessor
		title: 'Packages';
		allCandidates: [ self package correspondingRPackage 
			ifNotNil: [ :rpackage | { rpackage } ] 
			ifNil: [ { } ] ];
		itemName: [ :item | item name ];
		itemIcon: [ Smalltalk ui icons iconNamed: #packageIcon ];
		filter: GTFilterSubstring
]

{ #category : #'*GT-SpotterExtensions-Core' }
MCWorkingCopy >> spotterPackageRepositoriesFor: aStep [
	<spotterOrder: 10>
	aStep listProcessor
		title: 'Package repositories';
		allCandidates: [ self repositoryGroup repositories ];
		itemName: [ :item | item description ];
		actLogic: [ :item :step |
			step exit.
			self spotterCommit: self in: item ];
		filter: GTFilterSubstring
]
