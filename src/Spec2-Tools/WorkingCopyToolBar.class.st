"
WorkingCopyToolBar new openWithSpec
"
Class {
	#name : #WorkingCopyToolBar,
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
WorkingCopyToolBar class >> defaultSpec [ 

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
WorkingCopyToolBar >> browseButton [
	^ browseButton
]

{ #category : #accessing }
WorkingCopyToolBar >> configButton [
	
	^ configButton
]

{ #category : #initialization }
WorkingCopyToolBar >> createPackage [
	| name |
	name := UIManager default request: 'Name of package:'.
	name isEmptyOrNil
		ifTrue: [ ^ self ].
	RPackageOrganizer default registerPackageNamed: name.
	MCWorkingCopy forPackage: (MCPackage new name: name)
]

{ #category : #initialization }
WorkingCopyToolBar >> initializePresenters [

	browseButton := self newButton.
	configButton := self newButton.
	packageButton := self newButton.
		
	self setBrowseButton.
	self setConfigButton.
	self setPackageButton.
	self setSliceButton.
]

{ #category : #accessing }
WorkingCopyToolBar >> packageButton [

	^ packageButton
]

{ #category : #initialization }
WorkingCopyToolBar >> setBrowseButton [

	browseButton
		state: false;
		enabled: false;
		label: 'Browse';
		help: 'Browse the working copy of the selected package';
		action: [ self browseWorkingCopy ]
]

{ #category : #initialization }
WorkingCopyToolBar >> setConfigButton [

	configButton
		state: false;
		label: '+Config';
		help: 'Add a new metacello configuration';
		action: [ self createConfiguration ]
]

{ #category : #initialization }
WorkingCopyToolBar >> setPackageButton [

	packageButton
		state: false;
		label: '+Package';
		help: 'Add a new package and make it the working copy';
		action: [ self createPackage ]
]

{ #category : #initialization }
WorkingCopyToolBar >> setSliceButton [
	sliceButton := self instantiate: SpButtonPresenter.
	sliceButton
		state: false;
		label: '+Slice';
		help: 'Add a slice to cleaning and nicely propose a bug fix';
		action: [ self createSlice ]
]

{ #category : #accessing }
WorkingCopyToolBar >> sliceButton [
	
	^ sliceButton
]
