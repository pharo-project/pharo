Extension { #name : #DACompositeDependency }

{ #category : #'*Tool-DependencyAnalyser-UI' }
DACompositeDependency >> nodeClass [
	^ self dependencies first isMessageSendDependency
		ifTrue: [ DAManuallyAddedDependentPackageNode ]
		ifFalse: [ DADependentPackageNode ]
]
