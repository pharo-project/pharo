Extension { #name : #ComposablePresenter }

{ #category : #'*Commander-SpecSupport' }
ComposablePresenter >> buildCommanderMenuBy: aMenuCommandActivationClass [

	^aMenuCommandActivationClass buildContextMenuFor: self inContext: self createCommandContext
]

{ #category : #'*Commander-SpecSupport' }
ComposablePresenter >> createCommandContext [
	"Subclases should override it if they provide any selection"
	^self createCommandContextForSelection: #()
]

{ #category : #'*Commander-SpecSupport' }
ComposablePresenter >> createCommandContextForSelection: selectedItems [
	"simple context has no information about selected items"
	^CmdSimpleToolContext for: self
]

{ #category : #'*Commander-SpecSupport' }
ComposablePresenter >> enableCommanderMenu [

	self enableCommanderMenuBy: self
]

{ #category : #'*Commander-SpecSupport' }
ComposablePresenter >> enableCommanderMenuBy: presenterWithCommands [

	self menu: [ presenterWithCommands buildCommanderMenuBy: CmdContextMenuActivation]
]

{ #category : #'*Commander-SpecSupport' }
ComposablePresenter >> enableCommanderShortcutsIn: aWidget [

	^ CmdKMDispatcher attachedTo: aWidget withCommandsFrom: self
]
