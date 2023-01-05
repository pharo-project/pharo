Class {
	#name : #SycClassPatternPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'sourcePreviewPresenter',
		'sourceClassLabel',
		'targetClassLabel',
		'targetPreviewPresenter',
		'sourceClassPatternTextInput',
		'targetClassPatternTextInput',
		'sourcePackage'
	],
	#category : #'SystemCommands-RefactoringSupport'
}

{ #category : #examples }
SycClassPatternPresenter class >> example [

	| packageName |

	packageName := 'System-Support'.
	SycClassPatternPresenter openOn: packageName
]

{ #category : #'instance creation' }
SycClassPatternPresenter class >> openOn: aPackageName [
	"I take a RBMethodName as parameter and open the refactoring UI in a modal to rename it."

	| instance |
	instance := self on: aPackageName.
	^ instance openBlockedDialog
]

{ #category : #layout }
SycClassPatternPresenter >> defaultLayout [

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

{ #category : #initialization }
SycClassPatternPresenter >> initializeDialogWindow: aModalPresenter [

	aModalPresenter
		addButton: 'Ok' do: [ :presenter | self replaceClassPattern: presenter ];
		addButton: 'Cancel' do: [ :presenter | presenter beCancel; close ];
		initialExtent: 350 @ 500 ;
		title: 'Copy classes applying class pattern'
]

{ #category : #initialization }
SycClassPatternPresenter >> initializePresenters [

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
		yourself
]

{ #category : #initialization }
SycClassPatternPresenter >> replaceClassPattern: presenter [

	self shouldBeImplemented
]

{ #category : #'accessing - model' }
SycClassPatternPresenter >> setModelBeforeInitialization: aDomainObject [

	super setModelBeforeInitialization: aDomainObject.
	sourcePackage := aDomainObject
]

{ #category : #initialization }
SycClassPatternPresenter >> updateSourcePreview [

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

{ #category : #initialization }
SycClassPatternPresenter >> updateTargetPreview [

	| previewItems |
	previewItems := sourcePreviewPresenter items
				select: [ : className |
					[ className matchesRegex: targetClassPatternTextInput text ]
					on: RegexSyntaxError
					do: [ : ex | ex return: false ] ].
	previewItems
		ifEmpty: [ ^ targetPreviewPresenter items: #('(no classes matches the regex)') ].
	targetPreviewPresenter items: previewItems asSortedCollection
]
