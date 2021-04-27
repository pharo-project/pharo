"
A base for inspector toolbar commands.
"
Class {
	#name : #StInspectorCommand,
	#superclass : #CmCommand,
	#category : #'NewTools-Inspector-Command'
}

{ #category : #documentation }
StInspectorCommand class >> addDocumentContextRequiredSelectors: aBuilder [
	| requiredSelectors |

	requiredSelectors := self documentContextRequiredSelectors. 

	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Required Context API' ] withLevel: 2.	
	requiredSelectors 
		ifNotEmpty: [ 
			aBuilder text: 'To work properly, the context passed to this command has to implement the following vocabulary (See `CmCommand>>#context:`).'.
			aBuilder newLine.
			aBuilder unorderedListDuring: [  
				(requiredSelectors sorted) do: [ :each |
					aBuilder item: [
						aBuilder monospace: each ] ] ] ]
		ifEmpty: [ 
			aBuilder text: '(No required API is defined)' ]
]

{ #category : #documentation }
StInspectorCommand class >> buildMicroDownUsing: aBuilder withComment: aString [
	
	super buildMicroDownUsing: aBuilder withComment: aString.
	self addDocumentContextRequiredSelectors: aBuilder
]

{ #category : #initialization }
StInspectorCommand class >> defaultIconName [

	^ nil
]

{ #category : #default }
StInspectorCommand class >> defaultShortcutKey [

	^ nil
]

{ #category : #documentation }
StInspectorCommand class >> documentContextRequiredSelectors [
		
	^ #()
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
