Extension { #name : #SpDemoNotebookPresenter }

{ #category : #'*Spec2-Inspector' }
SpDemoNotebookPresenter >> objectInspectorPage [
	^ SpNotebookPage
		title: 'Object inspector'
		icon: (self iconNamed: #nautilusIcon)
		provider: [ EyeInspector new
				inspect: Object;
				yourself ]
		layoutSpec: #inspectorSpec
]
