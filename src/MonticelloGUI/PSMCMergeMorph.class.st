"
A PSMCMergeMorph is a graphical tool to navigate changes and select changes in case of conflicts.
"
Class {
	#name : 'PSMCMergeMorph',
	#superclass : 'ComposableMorph',
	#instVars : [
		'patchMorph',
		'codeMorph',
		'merged'
	],
	#category : 'MonticelloGUI-Diff',
	#package : 'MonticelloGUI',
	#tag : 'Diff'
}

{ #category : 'instance creation' }
PSMCMergeMorph class >> forMerger: aMerger [
	"Answer a new instance of the receiver
	with the given merger as the model."

	^ self new model: aMerger
]

{ #category : 'testing' }
PSMCMergeMorph >> allConflictsResolved [
	"Answer whether all conflicts were resolved."

	^(self model ifNil: [^false]) isMerged
]

{ #category : 'actions' }
PSMCMergeMorph >> cancel [
	"Delete the window to cancel."

	self window delete
]

{ #category : 'accessing' }
PSMCMergeMorph >> codeMorph [
	"Answer the value of codeMorph"

	^ codeMorph
]

{ #category : 'accessing' }
PSMCMergeMorph >> codeMorph: anObject [
	"Set the value of codeMorph"

	codeMorph := anObject
]

{ #category : 'actions' }
PSMCMergeMorph >> compositeText [
	"Answer the composite text from the patch morph."

	^self patchMorph compositeText
]

{ #category : 'accessing' }
PSMCMergeMorph >> compositeText: aString [
	"Save the new method text for the selected method.
	Yet to be implemented."

	self morphicUIManager inform: 'Saving of replacement methods is not yet imlemented.'
]

{ #category : 'accessing' }
PSMCMergeMorph >> conflictCount [
	"Answer the number of conflicts that are unresolved."

	^(self model ifNil: [^0]) conflicts count: [:c | c isResolved not]
]

{ #category : 'accessing' }
PSMCMergeMorph >> conflictCountString [
	"Answer a string describing the number of conflicts."

	|count|
	count := self conflictCount.
	^count = 1
		ifTrue: ['1 conflict' translated]
		ifFalse: ['{1} conflicts' translated format: {count}]
]

{ #category : 'accessing' }
PSMCMergeMorph >> defaultTitle [
	"Answer the default title label for the receiver."

	^'Merge' translated
]

{ #category : 'actions' }
PSMCMergeMorph >> fromDescription: aString [
	"Set the description for the left-hand side of the merge,
	typically 'currently in image'."

	self patchMorph fromDescription: aString
]

{ #category : 'initialization' }
PSMCMergeMorph >> initialize [
	"Initialize the receiver."

	|buttons buttonsHeight|
	super initialize.
	buttons := self newButtonsMorph.
	buttonsHeight := buttons minExtent y.
	self
		merged: false;
		patchMorph: self newPatchMorph;
		codeMorph: self newCodeMorph;
		changeProportionalLayout;
		addMorph: self patchMorph
		fullFrame: (0@0 corner: 1@0.6) asLayoutFrame ;
		addMorph: self codeMorph
		fullFrame: ((0@0.6 corner: 1@1) asLayoutFrame bottomOffset: buttonsHeight negated);
		addMorph: self newButtonsMorph
		fullFrame: ((0@1 corner: 1@1) asLayoutFrame topOffset: buttonsHeight negated);
		addPaneSplitters
]

{ #category : 'actions' }
PSMCMergeMorph >> merge [
	"Do the merge and close if no conflicts."

	^self model isMerged
		ifTrue: [self merged: true.
				self window delete]
		ifFalse: [self inform: 'You must resolve all conflicts first.' translated]
]

{ #category : 'accessing' }
PSMCMergeMorph >> merged [
	"Answer the value of merged. Indicates whether
	the merge button was pressed with no conflicts remaining."

	^ merged
]

{ #category : 'accessing' }
PSMCMergeMorph >> merged: anObject [
	"Set the value of merged"

	merged := anObject
]

{ #category : 'initialization' }
PSMCMergeMorph >> model: aMerger [
	"Set the model and update the window."

	|grouped sorted|
	super model: aMerger.
	grouped := aMerger operations
		groupedBy: [:o | o targetClassName ifNil: [o shortSummary]]
		having: [:g | true].
	sorted := OrderedCollection new.
	grouped keys asSortedCollection do: [:k |
		sorted addAll: ((grouped at: k) asSortedCollection: [:a :b | a shortSummary <= b shortSummary])].
	self patchMorph model: sorted
]

{ #category : 'instance creation' }
PSMCMergeMorph >> newButtonsMorph [
	"Answer a new buttons morph."

	^(self newRow: {
			self newConflictsButton.
			self newToolSpacer hResizing: #spaceFill.
			self newMergeButton.
			self newCancelButton})
		removeProperty: #fillStyle;
		listCentering: #bottomRight;
		layoutInset: 4
]

{ #category : 'instance creation' }
PSMCMergeMorph >> newCodeMorph [
	"Answer a new code morph."

	^(self newTextEditorFor: self
		getText: #compositeText
		setText: #compositeText:
		getEnabled: nil)
		wrapFlag: false;
		setText: ''
]

{ #category : 'instance creation' }
PSMCMergeMorph >> newConflictsButton [
	"Answer a new button for displaying the count
	of outstanding conflicts and navigating to each.
	It will be disabled if all conflicts are resolved."

	^(self
		newButtonFor: self
		getState: nil
		action:  #selectNextConflict
		arguments: #()
		getEnabled: #notAllConflictsResolved
		getLabel:  #conflictCountString
		help: 'Select the next conflict in the tree' translated)
		vResizing: #spaceFill "workaround until table  layout fixed"
]

{ #category : 'instance creation' }
PSMCMergeMorph >> newMergeButton [
	"Answer a new button for performing the merge.
	It will be disabled if any conflicts are unresolved."

	^self
		newButtonFor: self
		action:  #merge
		getEnabled:  #allConflictsResolved
		label:  'Merge' translated
		help: 'Merge the version into the image' translated
]

{ #category : 'instance creation' }
PSMCMergeMorph >> newPatchMorph [
	"Answer a new patch morph."

	^PSMCMergePatchMorph new
		borderWidth: 0;
		addDependent: self;
		yourself
]

{ #category : 'testing' }
PSMCMergeMorph >> notAllConflictsResolved [
	"Answer whether any conflicts are unresolved."

	^self allConflictsResolved not
]

{ #category : 'accessing' }
PSMCMergeMorph >> patchMorph [
	"Answer the value of patchMorph"

	^ patchMorph
]

{ #category : 'accessing' }
PSMCMergeMorph >> patchMorph: anObject [
	"Set the value of patchMorph"

	patchMorph := anObject
]

{ #category : 'actions' }
PSMCMergeMorph >> selectNextConflict [
	"Select the next conflict in the tree."

	self patchMorph selectNextConflict
]

{ #category : 'actions' }
PSMCMergeMorph >> toDescription: aString [
	"Set the description for the right-hand side of the merge,
	typically 'incoming'."

	self patchMorph toDescription: aString
]

{ #category : 'updating' }
PSMCMergeMorph >> update: aspect [
	"A join has probably changed its selection state."

	super update: aspect.
	aspect == #compositeText
		ifTrue: [self updateCode].
	aspect == #changes
		ifTrue: [self
				changed: #conflictCountString;
				changed: #allConflictsResolved;
				changed: #notAllConflictsResolved]
]

{ #category : 'updating' }
PSMCMergeMorph >> updateCode [
	"Update the code morph to match selected differences."

	self changed: #compositeText
]
