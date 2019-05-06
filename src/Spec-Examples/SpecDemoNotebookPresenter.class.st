Class {
	#name : #SpecDemoNotebookPresenter,
	#superclass : #ComposablePresenter,
	#instVars : [
		'notebook',
		'checkbox',
		'dynamicPage'
	],
	#category : #'Spec-Examples-Demo-Other'
}

{ #category : #specs }
SpecDemoNotebookPresenter class >> defaultSpec [
	^ SpecBoxLayout newVertical
		add: #notebook;
		add:
			(SpecBoxLayout newHorizontal
				add: #checkbox withConstraints: [ :constraints | constraints width: 20 ];
				add: 'Show dynamic presenter')
			withConstraints: [ :constraints | constraints height: self labelHeight ];
		yourself
]

{ #category : #pages }
SpecDemoNotebookPresenter >> browserPage [
	^ NotebookPage
		title: 'Browser'
		icon: (self iconNamed: #nautilusIcon)
		provider: [ ClassMethodBrowser new
				classes: self class environment allClasses;
				yourself ]
]

{ #category : #pages }
SpecDemoNotebookPresenter >> dynamicPage [
	^ NotebookPage title: 'Dynamic' icon: (self iconNamed: #nautilusIcon) provider: [ DynamicWidgetChange new ]
]

{ #category : #initialization }
SpecDemoNotebookPresenter >> initializePresenter [
	super initializePresenter.

	checkbox whenActivatedDo: [ notebook addPage: (dynamicPage := self dynamicPage) ].

	checkbox
		whenDeactivatedDo: [ dynamicPage
				ifNotNil: [ :page | 
					notebook removePage: page.
					dynamicPage := nil ] ]
]

{ #category : #initialization }
SpecDemoNotebookPresenter >> initializeWidgets [
	notebook := self newNotebook.
	checkbox := self newCheckBox.

	notebook
		addPage: self objectClassPage;
		addPage: self objectInspectorPage;
		addPage: self browserPage
]

{ #category : #pages }
SpecDemoNotebookPresenter >> objectClassPage [
	^ NotebookPage title: 'Object class' icon: (self iconNamed: #nautilusIcon) provider: [ MessageBrowser new messages: Object methods ]
]

{ #category : #pages }
SpecDemoNotebookPresenter >> objectInspectorPage [
	^ NotebookPage
		title: 'Object inspector'
		icon: (self iconNamed: #nautilusIcon)
		provider: [ EyeInspector new
				inspect: Object;
				yourself ]
		layoutSpec: #inspectorSpec
]
