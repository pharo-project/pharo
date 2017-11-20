"
I am a class dedicated to the transformation of a Komit object into a MCCommit and its submission
"
Class {
	#name : #MCKomitSubmitter,
	#superclass : #Object,
	#category : #Komitter-Models
}

{ #category : #'instance creation' }
MCKomitSubmitter class >> komit: komit [

	^ self new
		komit: komit;
		yourself
]

{ #category : #protocol }
MCKomitSubmitter >> commitKomit: aKomit [
	| versions |
	
	versions := self createVersionFor: aKomit.
	
	versions do: [ :version | 
		Cursor wait showWhile: [ 
			[ self
				storeVersion: version for: aKomit;
				storeDependencies: version  for: aKomit
			] ensure: [ (MCVersionInspector new version: version) show ] 
		]
	 ]
]

{ #category : #protocol }
MCKomitSubmitter >> commitSlice: aSlice [
	| versions slice view workingCopy version packages name |

	slice := MCSliceInfo new.
	view := KomitSliceUI new.
	view openDialogWithSpec
		centered;
		modalRelativeTo: World.

	view cancelled ifTrue: [ ^ self ].
	
	slice issueNumber: view issueNumberText.
	slice title: view titleText.
	
	versions := self createVersionFor: aSlice.
	
	KomitterManager current reset.
	
	packages := versions collect: [ :each | 
		KomitterManager current 
			storeVersion: each
			for: each package.
		each package ].
	
	slice includedPackages: packages.
	workingCopy := slice makeSlice.
	
	name := workingCopy silentUniqueVersionNameIn: aSlice remote remote.
	
	version := workingCopy 
		silentlyNewSliceVersionWithName: name 
		message: aSlice message 
		in: aSlice remote remote.
		
	Cursor wait showWhile: [
		[ self
			storeVersion: version for: aSlice;
			storeDependencies: version for: aSlice
		] ensure: [ (MCVersionInspector new version: version) show ] 
	]
]

{ #category : #protocol }
MCKomitSubmitter >> createVersionFor: aKomitableObject [
	| versions currentAuthor |
	currentAuthor := Author fullNamePerSe.
	Author fullName: aKomitableObject author.
	[ 
	versions := aKomitableObject entities keys
		collect: [ :each | 
			| workingCopy name version patcher |
			workingCopy := each package workingCopy.
			name := workingCopy silentUniqueVersionNameIn: aKomitableObject repository.
			version := workingCopy silentlyNewVersionWithName: name message: aKomitableObject message in: aKomitableObject repository.
			workingCopy modified: each isFullyCommited not.
			patcher := MCPatcher snapshot: each patch base.
			(aKomitableObject entities at: each) do: [ :e | e operation applyTo: patcher ].
			version := MCVersion
				package: version package
				info: version info
				snapshot: patcher patchedSnapshot
				dependencies: version dependencies.
			KomitPackage removePackage: each.
			version ] ]
		ensure: [ Author fullName: currentAuthor ].
	^ versions
]

{ #category : #private }
MCKomitSubmitter >> retryOnCredentialRequest: aBlock for: aRepository [

	aBlock on: MCPermissionDenied do: [ :error | |credentials| 
		credentials := MCCredentialsRequest 
							signalUrl: aRepository location
							username: aRepository user
							password: aRepository password.
		credentials 
			ifNotNil: [ 
				aRepository credentials: credentials.
				^ self retryOnCredentialRequest: aBlock for: aRepository ]]
]

{ #category : #private }
MCKomitSubmitter >> storeDependencies: newVersion for: aKomit [

	self 
		retryOnCredentialRequest: [ aKomit repository storeDependencies: newVersion ]
		for: aKomit repository
]

{ #category : #private }
MCKomitSubmitter >> storeVersion: newVersion for: aKomit [

	self 
		retryOnCredentialRequest: [ aKomit repository storeVersion: newVersion ]
		for: aKomit repository
]

{ #category : #protocol }
MCKomitSubmitter >> submitCommit: aKomit [

	MCSaveVersionDialog addAsLastLogMessage: aKomit message asString.
	aKomit commitOn: self
	
]
