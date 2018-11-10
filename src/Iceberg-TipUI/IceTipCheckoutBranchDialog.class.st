"
I'm a dialog to perform a checkout of a branch.

(self onRepository: IceRepository registry anyOne) openDialogWithSpec
"
Class {
	#name : #IceTipCheckoutBranchDialog,
	#superclass : #IceTipOptionDialog,
	#instVars : [
		'model',
		'types'
	],
	#category : #'Iceberg-TipUI-View-Branch'
}

{ #category : #'instance creation' }
IceTipCheckoutBranchDialog class >> onRepository: aRepository [
	^ self on: (IceTipRepositoryModel on: aRepository) beCached
]

{ #category : #specs }
IceTipCheckoutBranchDialog class >> title [
	^ 'Checkout branch'
]

{ #category : #accessing }
IceTipCheckoutBranchDialog >> allTypes [
	^ types ifNil: [ types := self createCheckoutBranchTypes ]
]

{ #category : #accessing }
IceTipCheckoutBranchDialog >> contextClass [
	^ IceTipBranchContext
]

{ #category : #'private factory' }
IceTipCheckoutBranchDialog >> createCheckoutBranchTypes [
	| allTypes |

	"Collect types new+local+remotes"	
	allTypes :=  { 
		IceTipCheckoutNewBranchPanel on: self model. "New branch"
	 	(IceTipCheckoutBranchPanel on: self model) 
			title: 'Local';
			icon: (self iconNamed: #branch);
			yourself }, 
	(self model remoteModels collect: [ :each | 
		(IceTipCheckoutBranchPanel on: each)
			title: each name;
			icon: (self iconNamed: #remote);
			yourself ]),
	(self model entity pluginManager checkoutBranchPanelsOnModel: self model).
	
	"Doing this because I can trigger the accept inside the panels."
	allTypes do: [ :each | each onAccept: [ self window delete ] ].
	
	^ allTypes
]

{ #category : #actions }
IceTipCheckoutBranchDialog >> doAccept [
	self selectedType doAccept
]

{ #category : #accessing }
IceTipCheckoutBranchDialog >> initialExtent [
	^ (600@400) scaledByDisplayScaleFactor
]

{ #category : #accessing }
IceTipCheckoutBranchDialog >> model [
	^ model
]

{ #category : #accessing }
IceTipCheckoutBranchDialog >> model: anObject [
	model := anObject.
	self allTypes do: [ :each | each model: anObject ]
]

{ #category : #initialization }
IceTipCheckoutBranchDialog >> setModelBeforeInitialization: anObject [ 
	model := anObject
]

{ #category : #accessing }
IceTipCheckoutBranchDialog >> title [
	^ 'Checkout ', super title asLowercase
]

{ #category : #accessing }
IceTipCheckoutBranchDialog >> withOnlyNewBranch [
	types := self allTypes select: #isNewBranch.
	typeList widget dataSource elements: types.
	typeList widget refresh
]

{ #category : #accessing }
IceTipCheckoutBranchDialog >> withTypes: aCollection [
	types := aCollection.
	typeList widget dataSource elements: types.
	typeList widget refresh
]

{ #category : #accessing }
IceTipCheckoutBranchDialog >> withoutNewBranch [
	types := self allTypes reject: #isNewBranch.
	typeList widget dataSource elements: types.
	typeList widget refresh
]
