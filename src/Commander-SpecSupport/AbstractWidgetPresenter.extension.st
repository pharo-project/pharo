Extension { #name : #AbstractWidgetPresenter }

{ #category : #'*Commander-SpecSupport' }
AbstractWidgetPresenter >> createDragTransferOf: items from: source withCommandsFrom: presenterWithCommands [

	| commandPassenger itemPassenger |
	commandPassenger := CmdDragAndDropActivation 
		createDragPassengerInContext: presenterWithCommands createCommandContext.
	itemPassenger := CmdSpecDragPassenger from: source model on: commandPassenger with: items.
	^self defaultTransferFor: itemPassenger from: source
]

{ #category : #'*Commander-SpecSupport' }
AbstractWidgetPresenter >> enableCommanderDragAndDropBy: presenterWithCommands [

	self enableCommanderDragBy: presenterWithCommands.
	self enableCommanderDropBy: presenterWithCommands
]

{ #category : #'*Commander-SpecSupport' }
AbstractWidgetPresenter >> enableCommanderDragBy: presenterWithCommands [

	self dragEnabled: true.
	self transferBlock: [ :items :source  | 
		self createDragTransferOf: items from: source withCommandsFrom: presenterWithCommands ].
]

{ #category : #'*Commander-SpecSupport' }
AbstractWidgetPresenter >> enableCommanderDropBy: aPresenter [

	self dropEnabled: true.
	self wantDropBlock: [ :draggedItem :event :source | 
		draggedItem canBeDroppedInContext: aPresenter createCommandContext ].
	self acceptDropBlock: [ :transfer :event :receiverMorph :receiverPresenter  | 
		transfer passenger dropInContext: (receiverMorph createCommandContextBy: aPresenter forDropAt: event)].
]
