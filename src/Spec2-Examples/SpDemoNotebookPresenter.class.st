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
		addPage: self objectInspectorPage;
		addPage: self browserPage
]

{ #category : #pages }
SpDemoNotebookPresenter >> objectClassPage [

	^ SpNotebookPage 
		title: 'Object class' 
		icon: (self iconNamed: #nautilusIcon) 
		provider: [ MessageBrowser new messages: Object methods ]
]

{ #category : #pages }
SpDemoNotebookPresenter >> objectInspectorPage [
	^ SpNotebookPage
		title: 'Object inspector'
		icon: (self iconNamed: #nautilusIcon)
		provider: [ 
			self flag: #TODO. "Using reference to suspend a dependency, but is not cool since 
			it makes a hidden depenency. Instead, we better think another example" 
			(self class environment at: #EyeInspector) new
				inspect: Object;
				yourself ]
		layoutSpec: #inspectorSpec
]
