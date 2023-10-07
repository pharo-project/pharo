"
Presents a two vertical pane dialog window to copy classes based in a regex pattern, using `RBClassRegexRefactoring` to preview changes.

- The left side of the window contains a text entry which expects a regex expression to match classes selected in an _input_ package.
- The right side of the window contains a text entry which expects a regex expression to match the classes resulted of the input list.

In both cases, a list for each of the selections is updated whenever the text entry changes.

See class side for an example.
"
Class {
	#name : 'SycCopyClassUsingPatternPresenter',
	#superclass : 'SpPresenter',
	#instVars : [
		'sourcePreviewPresenter',
		'sourceClassLabel',
		'targetClassLabel',
		'targetPreviewPresenter',
		'sourceClassPatternTextInput',
		'targetClassPatternTextInput',
		'sourcePackage',
		'newPackageName'
	],
	#category : 'SystemCommands-RefactoringSupport-View',
	#package : 'SystemCommands-RefactoringSupport',
	#tag : 'View'
}

{ #category : 'examples' }
SycCopyClassUsingPatternPresenter class >> example [

	| packageName |

	packageName := 'Shout' -> 'Shout2'.
	SycCopyClassUsingPatternPresenter openOn: packageName
]

{ #category : 'instance creation' }
SycCopyClassUsingPatternPresenter class >> openOn: aPackageNameAssociation [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."

	| instance |
	instance := self on: aPackageNameAssociation.
	^ instance openBlockedDialog
]

{ #category : 'layout' }
SycCopyClassUsingPatternPresenter >> defaultLayout [

	^ SpBoxLayout newLeftToRight
		spacing: 3;
		add: (	SpBoxLayout newTopToBottom
			spacing: 3;
			add: sourceClassLabel expand: false;
			add: sourceClassPatternTextInput expand: false;
			add: sourcePreviewPresenter;
			yourself);
		add: (SpBoxLayout newTopToBottom
			spacing: 3;
			add: targetClassLabel expand: false;
			add: targetClassPatternTextInput expand: false;
			add: targetPreviewPresenter;
			yourself);
		yourself
]

{ #category : 'initialization' }
SycCopyClassUsingPatternPresenter >> initializeDialogWindow: aModalPresenter [

	aModalPresenter
		addButton: 'Ok' do: [ :presenter |
			self replaceClassPattern: presenter.
			 presenter close ];
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		initialExtent: 350 @ 500 ;
		title: 'Copy classes applying class pattern'
]

{ #category : 'initialization' }
SycCopyClassUsingPatternPresenter >> initializePresenters [

	super initializePresenters.
	sourcePreviewPresenter := self newList.
	sourceClassLabel := self newLabel label: 'Source class pattern:'; yourself.
	sourceClassPatternTextInput := self newTextInput
		placeholder: '^RB(.*)$';
		whenTextChangedDo: [ :text | self updateSourcePreview ];
		yourself.

	targetPreviewPresenter := self newList.
	targetClassLabel := self newLabel label: 'Target class pattern:'; yourself.
	targetClassPatternTextInput := self newTextInput
		placeholder: 'Bar$1';
		whenTextChangedDo: [ : text | self updateTargetPreview ];
		yourself.

	self setFocus
]

{ #category : 'initialization' }
SycCopyClassUsingPatternPresenter >> replaceClassPattern: presenter [
	"Instantiate and execute a refactoring environment to perform changes according to receiver's current regex class patterns"

	(RBCopyPackageRefactoring copyPackage: sourcePackage packageName in: newPackageName)
		classMappings: (Dictionary newFromKeys: sourcePreviewPresenter items andValues: targetPreviewPresenter items);
		execute
]

{ #category : 'initialization' }
SycCopyClassUsingPatternPresenter >> setFocus [

	self focusOrder
		add: sourceClassPatternTextInput;
		add: targetClassPatternTextInput
]

{ #category : 'accessing - model' }
SycCopyClassUsingPatternPresenter >> setModelBeforeInitialization: aPackageNameAssociation [

	super setModelBeforeInitialization: aPackageNameAssociation.
	sourcePackage := aPackageNameAssociation key.
	newPackageName := aPackageNameAssociation value
]

{ #category : 'initialization' }
SycCopyClassUsingPatternPresenter >> updateSourcePreview [

	| previewItems |
	previewItems := sourcePackage classes
				collect: #name
				thenSelect: [ : className |
					[ className matchesRegex: sourceClassPatternTextInput text ]
					on: RegexSyntaxError
					do: [ : ex | ex return: false ] ].
	previewItems
		ifEmpty: [ ^ sourcePreviewPresenter items: #('(no classes matches the regex)') ].
	sourcePreviewPresenter items: previewItems asSortedCollection
]

{ #category : 'initialization' }
SycCopyClassUsingPatternPresenter >> updateTargetPreview [

	| previewItems |

	previewItems := sourcePreviewPresenter items collect: [ : className |
		RBClassRegexRefactoring new
			replace: sourceClassPatternTextInput text with: targetClassPatternTextInput text;
			execute: className ].
	previewItems
		ifEmpty: [ ^ targetPreviewPresenter items: #('(no classes matches the regex)') ].
	targetPreviewPresenter items: previewItems asSortedCollection
]
