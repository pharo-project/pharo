"
WorkingCopyToolBar new openWithSpec
"
Class {
	#name : #WorkingCopyToolbar,
	#superclass : #SpPresenter,
	#instVars : [
		'browseButton',
		'configButton',
		'packageButton',
		'sliceButton'
	],
	#category : #'Spec2-Tools-Monticello'
}

{ #category : #specs }
WorkingCopyToolbar class >> defaultSpec [ 

	^ {#SpContainerPresenter.
			#add: . {{#presenter . #packageButton.} . #layout: .  #(FrameLayout
										rightFraction: 0.25
										bottomFraction: 0
										bottomOffset: 25)} . 

			#add: . {{#presenter . #configButton.} .
						#layout: . #(FrameLayout
										leftFraction: 0.25
										rightFraction: 0.5
										bottomFraction: 0
										bottomOffset: 25) } .
											
			#add: . {{#presenter . #sliceButton.} .
						#layout: . #(FrameLayout
										leftFraction: 0.5
										rightFraction: 0.75
										bottomFraction: 0
										bottomOffset: 25) } .											

			#add: . {{#presenter . #browseButton.} .
						#layout: . #(FrameLayout
										leftFraction: 0.75
										bottomFraction: 0
										bottomOffset: 25)} 	
		}
]

{ #category : #accessing }
WorkingCopyToolbar >> browseButton [
	^ browseButton
]

{ #category : #accessing }
WorkingCopyToolbar >> configButton [
	
	^ configButton
]

{ #category : #initialization }
WorkingCopyToolbar >> createPackage [
	| name |
	name := UIManager default request: 'Name of package:'.
	name isEmptyOrNil
		ifTrue: [ ^ self ].
	RPackageOrganizer default registerPackageNamed: name.
	MCWorkingCopy forPackage: (MCPackage new name: name)
]

{ #category : #initialization }
WorkingCopyToolbar >> initializePresenters [

	browseButton := self newButton.
	configButton := self newButton.
	packageButton := self newButton.
		
	self setBrowseButton.
	self setConfigButton.
	self setPackageButton.
	self setSliceButton.
]

{ #category : #accessing }
WorkingCopyToolbar >> packageButton [

	^ packageButton
]

{ #category : #initialization }
WorkingCopyToolbar >> setBrowseButton [

	browseButton
		state: false;
		enabled: false;
		label: 'Browse';
		help: 'Browse the working copy of the selected package';
		action: [ self browseWorkingCopy ]
]

{ #category : #initialization }
WorkingCopyToolbar >> setConfigButton [

	configButton
		state: false;
		label: '+Config';
		help: 'Add a new metacello configuration';
		action: [ self createConfiguration ]
]

{ #category : #initialization }
WorkingCopyToolbar >> setPackageButton [

	packageButton
		state: false;
		label: '+Package';
		help: 'Add a new package and make it the working copy';
		action: [ self createPackage ]
]

{ #category : #initialization }
WorkingCopyToolbar >> setSliceButton [
	sliceButton := self instantiate: SpButtonPresenter.
	sliceButton
		state: false;
		label: '+Slice';
		help: 'Add a slice to cleaning and nicely propose a bug fix';
		action: [ self createSlice ]
]

{ #category : #accessing }
WorkingCopyToolbar >> sliceButton [
	
	^ sliceButton
]
