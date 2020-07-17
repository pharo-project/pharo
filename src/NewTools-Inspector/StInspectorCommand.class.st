Class {
	#name : #StInspectorCommand,
	#superclass : #CmCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #'instance creation' }
StInspectorCommand class >> toolbarCommandsFor: inspector [

	^ self
		instantiateCommandsFromPragma: #toolbarInspectCommand:
		for: inspector
]

{ #category : #testing }
StInspectorCommand >> appliesTo: anInspector [

	^ true
]

{ #category : #initialization }
StInspectorCommand >> initialize [

	super initialize.
	self description: (String streamContents: [ :stream | 
		stream 
			<< self class defaultName
			<< ': '
			<< self class defaultDescription ])	
]

{ #category : #execution }
StInspectorCommand >> interestingObject [

	^ self model inspectedObject
]

{ #category : #accessing }
StInspectorCommand >> model [

	^ self context model
]

{ #category : #accessing }
StInspectorCommand >> owner [ 
	
	^ self context owner
]
