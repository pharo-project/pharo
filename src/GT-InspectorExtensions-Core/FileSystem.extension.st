Extension { #name : #FileSystem }

{ #category : #'*GT-InspectorExtensions-Core' }
FileSystem >> gtInspectorRootItemsIn: composite [
	<gtInspectorPresentationOrder: 40>
	(self root gtInspectorItemsIn: composite)
		title: 'Root directory'
]

{ #category : #'*GT-InspectorExtensions-Core' }
FileSystem >> gtInspectorWorkingDirectoryItemsIn: composite [
	<gtInspectorPresentationOrder: 40>
	(self workingDirectory gtInspectorItemsIn: composite)
		title: 'Working directory'
]
