"
Description
--------------------

I am a presenter to show how to use notebook with spec.
"
Class {
	#name : #SpDemoNotebookPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'notebook',
		'checkbox',
		'dynamicPage'
	],
	#category : #'Spec2-Examples-Demo-Other'
}

{ #category : #specs }
SpDemoNotebookPresenter class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #notebook;
		add:
			(SpBoxLayout newHorizontal
				add: #checkbox withConstraints: [ :constraints | constraints width: 20 ];
				add: 'Show dynamic presenter')
			withConstraints: [ :constraints | constraints height: self labelHeight ];
		yourself
]

{ #category : #examples }
SpDemoNotebookPresenter class >> example [
	^ self new openWithSpec
]

{ #category : #pages }
SpDemoNotebookPresenter >> browserPage [
	^ SpNotebookPage
		title: 'Browser'
		icon: (self iconNamed: #nautilusIcon)
		provider: [ SpClassMethodBrowser new
				classes: self class environment allClasses;
				yourself ]
]

{ #category : #initialization }
SpDemoNotebookPresenter >> connectPresenters [
	super connectPresenters.

	checkbox whenActivatedDo: [ notebook addPage: (dynamicPage := self dynamicPage) ].

	checkbox
		whenDeactivatedDo: [ dynamicPage
				ifNotNil: [ :page | 
					notebook removePage: page.
					dynamicPage := nil ] ]
]

{ #category : #pages }
SpDemoNotebookPresenter >> dynamicPage [
	^ SpNotebookPage title: 'Dynamic' icon: (self iconNamed: #nautilusIcon) provider: [ SpDynamicWidgetChange new ]
]

{ #category : #initialization }
SpDemoNotebookPresenter >> initializePresenters [
	notebook := self newNotebook.
	checkbox := self newCheckBox.
	notebook
		addPage: self objectClassPage;
		addPage: self browserPage.

	self class methods
		select: [ :aMethod | aMethod isExtension and: [ aMethod selector endsWith: 'Page' ] ]
		thenDo: [ :aMethod | notebook addPage: (self perform: aMethod selector ) ]
]

{ #category : #pages }
SpDemoNotebookPresenter >> objectClassPage [
	^ SpNotebookPage title: 'Object class' icon: (self iconNamed: #nautilusIcon) provider: [ MessageBrowser new messages: Object methods ]
]
