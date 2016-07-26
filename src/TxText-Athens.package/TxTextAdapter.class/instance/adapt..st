as yet unclassified
adapt: aComposableModel

	model := aComposableModel.
	aComposableModel addDependent: self.

	widget := self buildWidget.
	
	model whenTextChanged: [ :new :old | 
		self editorDo: [ :e | e contents: new ]
		 ].
	"model text whenChangedSend: #setNewText: to: self"