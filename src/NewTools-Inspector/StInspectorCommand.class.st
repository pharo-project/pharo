Class {
	#name : #StInspectorCommand,
	#superclass : #CmCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #initialization }
StInspectorCommand class >> defaultIconName [

	^ nil
]

{ #category : #default }
StInspectorCommand class >> defaultShortcutKey [

	^ nil
]

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

{ #category : #converting }
StInspectorCommand >> asSpecCommand [
	| command |
	
	command := super asSpecCommand.
	self shortcutKey ifNotNil: [ :key | command shortcutKey: key ].
	self defaultIconName
		ifNotNil: [ :iconName | command iconName: iconName ].
	
	^ command
]

{ #category : #converting }
StInspectorCommand >> defaultIconName [

	^ self class defaultIconName
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

{ #category : #converting }
StInspectorCommand >> shortcutKey [

	^ self class defaultShortcutKey
]
