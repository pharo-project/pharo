"
I expose data of a branch.
"
Class {
	#name : #IceTipBranchModel,
	#superclass : #IceTipCommitishModel,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #private }
IceTipBranchModel >> addDecorationTo: aString [
	| text |
	text := aString asText.
	[ 
		self entity isHead ifTrue: [ text := text allBold ] ]
	on: IceNotInBranch do: [ :e | "Do not decorate" ].
	^ text
]

{ #category : #accessing }
IceTipBranchModel >> checkoutStrategies [
	^ IceCheckoutStrategy allStrategies
]

{ #category : #actions }
IceTipBranchModel >> checkoutWithStrategy: aCheckoutStrategy then: aBlock [
	<noCache>
	
	IceTipStandardAction new 
		repository: self entity repository; 
		message: ('Checking out branch {1} from {2}' format: { self name. self repositoryName });
		onSuccessRepositoryModified;
		onSuccess: aBlock;
		execute: [ self entity checkoutWithStrategy: aCheckoutStrategy ]
]

{ #category : #accessing }
IceTipBranchModel >> commitModels [
	| commits |
	commits := OrderedCollection new.
	self entity commitsDo: [ :each | 
		commits add: (IceTipCommitModel 
			repositoryModel: self repositoryModel 
			on: each) 
			beCached ].
	^ commits
]

{ #category : #accessing }
IceTipBranchModel >> commits [
	self shouldBeImplemented.
]

{ #category : #actions }
IceTipBranchModel >> delete [
	<noCache>
	
	IceTipStandardAction new
		repository: self repositoryModel entity;
		message: ('Removing branch {1}' format: { self name });
		onSuccessRepositoryModified;
		execute: [ self entity delete ]
]

{ #category : #accessing }
IceTipBranchModel >> descriptionWithDecoration [
	^ self addDecorationTo: (self entity isRemote 
		ifTrue: [ self entity shortName ]
		ifFalse: [ self name ])
]

{ #category : #accessing }
IceTipBranchModel >> diffFromHead [
	| commitModel |
	commitModel := IceTipCommitModel on: self entity commit.
	^ commitModel diffFromHead
]

{ #category : #accessing }
IceTipBranchModel >> diffToWorkingCopy [
	| commitModel |
	commitModel := IceTipCommitModel on: self entity commit.
	^ commitModel diffToWorkingCopy
]

{ #category : #testing }
IceTipBranchModel >> hasUpstream [
	
	^ entity hasUpstream
]

{ #category : #testing }
IceTipBranchModel >> isHead [
	^ self entity isHead
]

{ #category : #testing }
IceTipBranchModel >> isLocal [
	^ self entity isLocal
]

{ #category : #actions }
IceTipBranchModel >> mergeThen: aBlock [
	<noCache>

	IceTipStandardAction new 
		repository: self repositoryModel entity;
		message: ('Merging branch {1} from {2}' format: { self name. self repositoryName });
		onSuccessRepositoryModified;
		onSuccess: aBlock;
		execute: [ self entity merge ]
]

{ #category : #accessing }
IceTipBranchModel >> name [
	^ self entity name
]

{ #category : #actions }
IceTipBranchModel >> previewCheckout [
	"this will open a 'checkout browser' that will allow user to validate the changes before 
	 actually checking out (checkout operation will be called in my sibling method, #checkout)"
	<noCache>

	(IceTipCheckoutPreviewBrowser onBranch: self) openWithSpec
]

{ #category : #actions }
IceTipBranchModel >> previewMerge: aMergeType [
	"this will open a 'checkout browser' that will allow user to validate the changes before 
	 actually checking out (checkout operation will be called in my sibling method, #checkout)"
	<noCache>
	(IceTipMergePreviewBrowser onBranch: self) 
		mergeType: aMergeType;
		openWithSpec
]

{ #category : #printing }
IceTipBranchModel >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $[;
		nextPutAll: self name;
		nextPut: $]
]

{ #category : #accessing }
IceTipBranchModel >> repositoryName [
	^ self repositoryModel name
]

{ #category : #accessing }
IceTipBranchModel >> shortDescriptionWithDecoration [
	^ self addDecorationTo: (self entity isRemote 
		ifTrue: [ self name copyAfter: $/ ]
		ifFalse: [ self name])
]

{ #category : #actions }
IceTipBranchModel >> switchAndMergeThen: aBlock [
	<noCache>

	IceTipStandardAction new
		repository: self entity repository; 
		message: ('Merging branch {1} from {2}' format: { self name. self repositoryName });
		onSuccessRepositoryModified;
		onSuccess: aBlock;
		execute: [ self entity switchAndMergeWorkingCopy ]
]
