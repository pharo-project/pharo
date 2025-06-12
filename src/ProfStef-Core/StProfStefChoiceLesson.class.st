Class {
	#name : 'StProfStefChoiceLesson',
	#superclass : 'SpPresenter',
	#instVars : [
		'listLesson',
		'parent',
		'okButton',
		'profInstance',
		'selectedLesson',
		'cancelButton'
	],
	#category : 'ProfStef-Core-presenter',
	#package : 'ProfStef-Core',
	#tag : 'presenter'
}

{ #category : 'as yet unclassified' }
StProfStefChoiceLesson class >> profInst: aProfStefInst model: anModelPresenter [ 
	^ self basicNew 
	profStefInst: ProfStef default;
	setModelBeforeInitialization: anModelPresenter;
	initialize;
	yourself.
]

{ #category : 'layout' }
StProfStefChoiceLesson >> defaultLayout [ 
	^ (SpBoxLayout newTopToBottom 
		add: listLesson;
		add: (SpBoxLayout newLeftToRight hAlignEnd;
					add: cancelButton;
					add: okButton;
				
					yourself) withConstraints: [ :constraint | constraint width: 90 ; height: 30 ] ;
				
	
	yourself).
]

{ #category : 'initialization' }
StProfStefChoiceLesson >> initialize [
	super initialize.
]

{ #category : 'initialization' }
StProfStefChoiceLesson >> initializeButtons [ 
	okButton := self newButton.
	okButton label: 'go to' .
	okButton action: [ self okAction ].
	
	cancelButton := self newButton.
	cancelButton label: 'cancel'.
	cancelButton action: [ self delete ].
]

{ #category : 'initialization' }
StProfStefChoiceLesson >> initializeListLesson [
	listLesson := self newColumnView.
	selectedLesson := profInstance player tutorial tutorial first.
	
	listLesson addColumnTitle: 'list of lesson'
		setup: [ :aPresenter | aPresenter newLabel ]
		bind: [ :aPresenter :aLesson | aPresenter label: aLesson asString ];
		whenSelectedDo: [ :selection |
				selectedLesson := selection.
		];
		items: profInstance player tutorial tutorial.
]

{ #category : 'initialization' }
StProfStefChoiceLesson >> initializePresenter [ 
	self initializeListLesson.
	self initializeButtons.

	
	
]

{ #category : 'layout' }
StProfStefChoiceLesson >> initializeWindow: aWindowPresenter [
	aWindowPresenter initialExtent: 500@200;
	title: self windowTitle.
]

{ #category : 'api' }
StProfStefChoiceLesson >> okAction [ 
	profInstance tutorial: profInstance player tutorial class lesson: selectedLesson.
	self delete.
]

{ #category : 'initialization' }
StProfStefChoiceLesson >> profStefInst: aProfStef [
	profInstance := aProfStef 
]

{ #category : 'layout' }
StProfStefChoiceLesson >> setModelBeforeInitialization: aModel [
	parent := aModel.
]

{ #category : 'layout' }
StProfStefChoiceLesson >> windowTitle [ 
	^ 'lesson choise'
]
