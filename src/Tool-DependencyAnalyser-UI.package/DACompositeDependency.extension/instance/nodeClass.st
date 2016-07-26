*Tool-DependencyAnalyser-UI
nodeClass
	^ self dependencies first isMessageSendDependency
		ifTrue: [ DAManuallyAddedDependentPackageNode ]
		ifFalse: [ DADependentPackageNode ]