Class {
	#name : 'StProfStefLessonChooser',
	#superclass : 'SpPresenter',
	#instVars : [
		'listLesson',
		'parent',
		'okButton',
		'selectedLesson',
		'cancelButton',
		'profStef'
	],
	#category : 'ProfStef-Core-Presenter',
	#package : 'ProfStef-Core',
	#tag : 'Presenter'
}

{ #category : 'private - accessing' }
StProfStefLessonChooser class >> model: aModel [
	^ self basicNew 
	profStef: ProfStef default;
	setModelBeforeInitialization: aModel;
	initialize;
	yourself.
]

{ #category : 'layout' }
StProfStefLessonChooser >> defaultLayout [ 
	^ (SpBoxLayout newTopToBottom 
		add: listLesson;
		add: (SpBoxLayout newLeftToRight hAlignEnd;
					add: cancelButton;
					add: okButton;
				
					yourself) withConstraints: [ :constraint | constraint width: 90 ; height: 30 ] ;
				
	
	yourself).
]

{ #category : 'initialization' }
StProfStefLessonChooser >> initializeButtons [ 
	okButton := self newButton.
	okButton label: 'go to' .
	okButton action: [ self okAction ].
	
	cancelButton := self newButton.
	cancelButton label: 'cancel'.
	cancelButton action: [ self delete ].
]

{ #category : 'initialization' }
StProfStefLessonChooser >> initializeListLesson [
	listLesson := self newColumnView.
	selectedLesson := profStef player tutorial tutorial first.
	
	listLesson addColumnTitle: 'list of lesson'
		setup: [ :aPresenter | aPresenter newLabel ]
		bind: [ :aPresenter :aLesson | aPresenter label: aLesson asString ];
		whenSelectedDo: [ :selection |
				selectedLesson := selection.
		];
		items: profStef player tutorial tutorial.
]

{ #category : 'initialization' }
StProfStefLessonChooser >> initializePresenter [ 
	self initializeListLesson.
	self initializeButtons.

	
	
]

{ #category : 'layout' }
StProfStefLessonChooser >> initializeWindow: aWindowPresenter [
	aWindowPresenter initialExtent: 500@200;
	title: self windowTitle.
]

{ #category : 'api' }
StProfStefLessonChooser >> okAction [ 
	profStef tutorial: profStef player tutorial class lesson: selectedLesson.
	self delete.
]

{ #category : 'initialization' }
StProfStefLessonChooser >> profStef: aProfStef [
	profStef := aProfStef 
]

{ #category : 'layout' }
StProfStefLessonChooser >> setModelBeforeInitialization: aModel [
	parent := aModel.
]

{ #category : 'layout' }
StProfStefLessonChooser >> windowTitle [ 
	^ 'lesson chooser'
]
