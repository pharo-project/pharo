"
I am a presenter that allows to select items from a list of packages in my left column and put that items on a selected list in my rigth column. I support selection for checkBox. 

If you click on 'Next', I will open the CBSelectRule.
I send selected packages from selected list to CBCritiqueBrowser. 

You can open me whit:
`CBSelectPackage open`


"
Class {
	#name : #CBSelectPackage,
	#superclass : #SpPresenter,
	#instVars : [
		'nextButton',
		'chooserPackages'
	],
	#category : #'Tool-CriticBrowser-Applier'
}

{ #category : #'instance creation' }
CBSelectPackage class >> open [

	<example>
	^ self new openWithSpec
]

{ #category : #'as yet unclassified' }
CBSelectPackage >> chooserPackages [

	^chooserPackages 
]

{ #category : #initialization }
CBSelectPackage >> initializeLayout [

	self layout: (SpBoxLayout newTopToBottom
			add: #chooserPackages;
			add: (SpBoxLayout newLeftToRight
					  addLast: #nextButton;
					  yourself)
			 height: 25;
			 yourself)
]

{ #category : #initialization }
CBSelectPackage >> initializeNextButton [

	nextButton := self newButton.
	nextButton
		icon: (self iconNamed: #smallOk);
		label: 'Next';
		action: [ self nextAction ]
]

{ #category : #initialization }
CBSelectPackage >> initializePresenters [

	chooserPackages := self setChooserPackages.
	self initializeNextButton.
	self initializeLayout
]

{ #category : #initialization }
CBSelectPackage >> initializeWindow: aWindowPresenter [

	aWindowPresenter 
		title: self title;
		initialExtent: 450@360
]

{ #category : #adding }
CBSelectPackage >> nextAction [

	| packages env |
	packages := chooserPackages targetList items.
	env := RBPackageEnvironment new packages: packages.
	CBSelectRule openWithEnvironment: env removeTestCase: false.
	self delete
]

{ #category : #initialization }
CBSelectPackage >> nextButton [

	^nextButton
]

{ #category : #initialization }
CBSelectPackage >> setChooserPackages [
	
	| chooser |
	chooser := SpChooserPresenter
		   sourceItems: RPackageOrganizer default packages
		   chosenItems:
		   {  }
		   displayBlock: #packageName.
	chooser sourceList displayIcon: [ self iconNamed: #packageIcon ].
	chooser targetList displayIcon: [ self iconNamed: #packageIcon ].
	^ chooser 
]

{ #category : #initialization }
CBSelectPackage >> title [

	^ 'Select packages'
]
