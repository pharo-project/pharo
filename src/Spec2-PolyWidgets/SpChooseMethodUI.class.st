"
I am an UI used to select a method of the system. 

I'll display the list of packages of the system, the classes of the selected package and the methods of the selected class. 

The user can configure me with an action to execute when the user does the selection. The action will take my instance as parameter to get the package/class/method selected.

Public API and Key Messages
--------------------

- class>>#openOnAccept: aBlock 		This method will open me in a modal and execute the block as parameter when the user accept.

Examples
--------------------

	(self openOnAccept: [ :chooseMethodPresenter | chooseMethodPresenter selectedMethod browse ])
		withWindowDo: [ :w | w title: 'Select a method example' ]
	
 
Internal Representation and Key Implementation Points.
--------------------

    Instance Variables
	packageList:						<aListPresenter>	A list of the packages of the system.
	classList:						<aListPresenter>	A list of the classes in the selected package.
	methodList:						<aListPresenter>	A list of the methods in the selected class.
	radioButtonClassSide:			<aRadioButton>		A radio button to select the class side. 
	radioButtonInstanceSide:		<aRadioButton>		A radio button to select the instance side.

"
Class {
	#name : #SpChooseMethodUI,
	#superclass : #SpPresenter,
	#instVars : [
		'packageList',
		'classList',
		'methodList',
		'radioButtonInstanceSide',
		'radioButtonClassSide'
	],
	#category : #'Spec2-PolyWidgets-Widgets'
}

{ #category : #specs }
SpChooseMethodUI class >> defaultSpec [
	^ SpPanedLayout newHorizontal
		position: 33 percent;
		add: #packageList;
		add:
			(SpPanedLayout newHorizontal
				position: 50 percent;
				add: #classList;
				add:
					(SpBoxLayout newVertical
						add: #methodList;
						add:
							(SpBoxLayout newHorizontal
								add: #radioButtonInstanceSide;
								add: #radioButtonClassSide;
								yourself)
							withConstraints: [ :contraint | contraint height: 20 ];
						yourself);
				yourself);
		yourself
]

{ #category : #examples }
SpChooseMethodUI class >> example [
	<example>
	(self openOnAccept: [ :chooseMethodPresenter | chooseMethodPresenter selectedMethod browse ]) withWindowDo: [ :w | w title: 'Select a method example' ]
]

{ #category : #'instance creation' }
SpChooseMethodUI class >> openOnAccept: aBlock [
	| instance |
	instance := self new.
	instance openModalWithSpec.
	instance withWindowDo: [ :w | w okAction: [ :dialog | aBlock cull: instance cull: dialog ] ].
	^ instance
]

{ #category : #accessing }
SpChooseMethodUI >> classList [
	^ classList
]

{ #category : #utilities }
SpChooseMethodUI >> classMethodsOf: aClass [
	^ aClass class methods
]

{ #category : #initialization }
SpChooseMethodUI >> initializeDialogWindow: aDialog [
	super initializeDialogWindow: aDialog.
	aDialog
		initialExtent: 600 @ 400;
		title: 'Select a method';
		closeOnBackdropClick: false
]

{ #category : #initialization }
SpChooseMethodUI >> initializePresenter [
	packageList
		transmitTo: classList
		transform: [ :selectedPacakge | selectedPacakge classes asOrderedCollection ]
		postTransmission: [ classList selectIndex: 1 ].

	classList
		transmitTo: methodList
		transform: [ :selectedClass | selectedClass ifNotNil: [ self methodsToDisplayOf: selectedClass ] ifNil: [ #() ] ]
		postTransmission: [ :destination | destination selectIndex: 1 ].

	radioButtonClassSide whenActivatedDo: [ methodList items: (self classMethodsOf: self selectedClass) ].

	radioButtonInstanceSide whenActivatedDo: [ methodList items: (self instanceMethodsOf: self selectedClass) ].

	packageList selectIndex: 1
]

{ #category : #initialization }
SpChooseMethodUI >> initializeWidgets [
	packageList := self newList.
	classList := self newList.
	methodList := self newList.
	radioButtonClassSide := self newRadioButton.
	radioButtonInstanceSide := self newRadioButton.

	packageList
		items: RPackageOrganizer default packages;
		displayBlock: #name;
		sortingBlock: #name ascending;
		icons: [ :item | self iconNamed: #package ].

	classList
		sortingBlock: #name ascending;
		icons: [ :item | self iconNamed: item systemIconName ].

	methodList
		sortingBlock: #selector ascending;
		displayBlock: #selector.

	radioButtonClassSide label: 'class side'.
	radioButtonInstanceSide
		label: 'instance side';
		state: true.

	radioButtonInstanceSide associatedRadioButtons: {radioButtonClassSide}.

	self focusOrder
		add: packageList;
		add: classList;
		add: methodList
]

{ #category : #utilities }
SpChooseMethodUI >> instanceMethodsOf: aClass [
	^ aClass methods
]

{ #category : #testing }
SpChooseMethodUI >> isInstanceButtonSelected [
	^ radioButtonInstanceSide state
]

{ #category : #accessing }
SpChooseMethodUI >> methodList [
	^ methodList
]

{ #category : #utilities }
SpChooseMethodUI >> methodsToDisplayOf: aClass [
	^ (self isInstanceButtonSelected ifTrue: [ self instanceMethodsOf: aClass ] ifFalse: [ self classMethodsOf: aClass ]) asOrderedCollection
]

{ #category : #accessing }
SpChooseMethodUI >> packageList [
	^ packageList
]

{ #category : #accessing }
SpChooseMethodUI >> radioButtonClassSide [
	^ radioButtonClassSide
]

{ #category : #accessing }
SpChooseMethodUI >> radioButtonInstanceSide [
	^ radioButtonInstanceSide
]

{ #category : #accessing }
SpChooseMethodUI >> selectedClass [
	^ classList selectedItem
]

{ #category : #accessing }
SpChooseMethodUI >> selectedMethod [
	^ methodList selectedItem
]

{ #category : #accessing }
SpChooseMethodUI >> selectedPackage [
	^ packageList selectedItem
]
