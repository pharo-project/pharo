Class {
	#name : 'StProfStefChoiseLesson',
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
StProfStefChoiseLesson class >> profInst: aProfStefInst model: anModelPresenter [ 
	^ self basicNew 
	profStefInst: ProfStef default;
	setModelBeforeInitialization: anModelPresenter;
	initialize;
	yourself.
]

{ #category : 'layout' }
StProfStefChoiseLesson >> defaultLayout [ 
	^ (SpBoxLayout newTopToBottom 
		add: listLesson;
		add: (SpBoxLayout newLeftToRight hAlignEnd;
					add: cancelButton;
					add: okButton;
				
					yourself) withConstraints: [ :constraint | constraint width: 90 ; height: 30 ] ;
				
	
	yourself).
]

{ #category : 'initialization' }
StProfStefChoiseLesson >> initialize [
	super initialize.
]

{ #category : 'initialization - deprecated' }
StProfStefChoiseLesson >> initializePresenter [ 
	listLesson := self newColumnView.
	selectedLesson := profInstance player tutorial tutorial first.
	
	listLesson addColumnTitle: 'list of lesson'
		setup: [ :aPresenter | aPresenter newLabel ]
		bind: [ :aPresenter :aLesson | aPresenter label: aLesson asString ];
		whenSelectedDo: [ :selection |
				selectedLesson := selection.
		];
		items: profInstance player tutorial tutorial.

	okButton := self newButton.
	okButton label: 'go to' .
	okButton extent: 100@10.
	okButton action: [ self okAction ].
	
	cancelButton := self newButton.
	cancelButton label: 'cancel'.
	cancelButton extent: 60@10.
	cancelButton action: [ self delete ]
	
]

{ #category : 'layout' }
StProfStefChoiseLesson >> initializeWindow: aWindowPresenter [
	aWindowPresenter initialExtent: 500@200;
	title: self windowTitle.
]

{ #category : 'api' }
StProfStefChoiseLesson >> okAction [ 
	profInstance tutorial: profInstance player tutorial class lesson: selectedLesson.
	self delete.
]

{ #category : 'as yet unclassified' }
StProfStefChoiseLesson >> profStefInst: aProfStef [
	profInstance := aProfStef 
]

{ #category : 'layout' }
StProfStefChoiseLesson >> setModelBeforeInitialization: aModel [
	parent := aModel.
]

{ #category : 'layout' }
StProfStefChoiseLesson >> windowTitle [ 
	^ 'lesson choise'
]
