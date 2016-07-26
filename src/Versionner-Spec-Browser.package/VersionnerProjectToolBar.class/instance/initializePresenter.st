initialization
initializePresenter
	"Initialize widgets behavior"

	updateButton action: [  self browser hasChanges 
								ifTrue: [ self executeWithDevWorkflow: VSUpdateDevelopmentVersionCommand ]
								ifFalse: [ UIManager default inform: 'Nothing to save' ] ].
	
	releaseButton action: [ self browser ignoreUnsavedChangesOnDevelopmentIfAny
								ifTrue: [ self executeWithDevWorkflow: VSReleaseDevelopmentVersionCommand ] ].
	
	commitButton action: [ (VSSaveProjectCommand target: browser project for: self) execute ].