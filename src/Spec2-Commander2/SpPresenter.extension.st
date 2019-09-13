Extension { #name : #SpPresenter }

{ #category : #'*Spec2-Commander2' }
SpPresenter class >> buildCommandsGroupWith: presenter forRoot: aCmCommandsGroup [
	"This hook allows you to build your groups of commands.
	 You must attach them to aLtCommandsGroup throught the registration mechanism.
	 The presenter instance to which this group of command will be bound is provided (this is useful for setting commands context).
	 To be overrided by subclasses.
	"
]

{ #category : #'*Spec2-Commander2' }
SpPresenter class >> buildRootCommandsGroupFor: presenterInstance [
	| rootCommandGroup |
	rootCommandGroup := CmCommandGroup forSpec beRoot.
	"Register default commands."
	self buildCommandsGroupWith: presenterInstance forRoot: rootCommandGroup.
	
	"Register extension commands."
	(Pragma allNamed: #extensionCommands in: self class) do: [ :pragma | 
		(self 
			perform: pragma methodSelector 
			withArguments: { presenterInstance . rootCommandGroup }) ].
	
	^ rootCommandGroup
]

{ #category : #'*Spec2-Commander2' }
SpPresenter >> defaultCommandsContext [
	"The default context set to commands is the presenter itself.
	 This method might be overriden to change this behaviour.
	"
	^ self
]

{ #category : #'*Spec2-Commander2' }
SpPresenter >> rootCommandsGroup [
	^ SpRecursiveContextSetter visit: (self class buildRootCommandsGroupFor: self defaultCommandsContext) toSetContext: self defaultCommandsContext
]
