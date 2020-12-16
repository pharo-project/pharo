"
Monticello's dialog that allows the user to change the version name and to enter a message for the commit log.
"
Class {
	#name : #MCSaveVersionDialog,
	#superclass : #MCTool,
	#instVars : [
		'name',
		'message'
	],
	#classVars : [
		'PreviousMessages'
	],
	#category : #'MonticelloGUI-Tools'
}

{ #category : #history }
MCSaveVersionDialog class >> addAsLastLogMessage: aString [

	((self previousMessages size > 0) and: [self previousMessages first = aString])
		ifTrue: [ ^ self ].
		
	self previousMessages addFirst: aString.
	(self previousMessages size > self maxLogMessageHistory)
		ifTrue: [self previousMessages removeLast]
]

{ #category : #history }
MCSaveVersionDialog class >> maxLogMessageHistory [

	^ 15
]

{ #category : #history }
MCSaveVersionDialog class >> previousMessages [

	^ PreviousMessages ifNil: [ PreviousMessages := OrderedCollection new]
]

{ #category : #icons }
MCSaveVersionDialog class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#smallSaveAsIcon
]

{ #category : #'morphic ui' }
MCSaveVersionDialog >> accept [
	| version aMessage |
	version := (self findTextMorph: #versionName) text asString.
	aMessage := (self findTextMorph: #logMessage) text asString.
	self addAsLastLogMessage: aMessage.
	self answer: {version. aMessage}
]

{ #category : #'log message history' }
MCSaveVersionDialog >> addAsLastLogMessage: aString [

	self class addAsLastLogMessage: aString
]

{ #category : #'morphic ui' }
MCSaveVersionDialog >> buttonSpecs [
	^ #((Accept accept 'accept version name and log message')
		(Cancel cancel 'cancel saving version')
		('Old log messages...' oldLogMessages 're-use a previous log message')
		) 
]

{ #category : #'morphic ui' }
MCSaveVersionDialog >> cancel [
	self answer: nil
]

{ #category : #'morphic ui' }
MCSaveVersionDialog >> defaultLabel [
	^ 'Edit Version Name and Log Message:'
]

{ #category : #'morphic ui' }
MCSaveVersionDialog >> initialExtent [ 
	^ 400@300
]

{ #category : #accessing }
MCSaveVersionDialog >> logMessage [
	^ message ifNil: ['empty log message']
]

{ #category : #accessing }
MCSaveVersionDialog >> logMessage: aString [
	message := aString.
	self changed: #logMessage
]

{ #category : #'log message history' }
MCSaveVersionDialog >> maxLogMessageHistory [

	^ self maxLogMessageHistory
]

{ #category : #'morphic ui' }
MCSaveVersionDialog >> oldLogMessages [
	| list index |
	list := self previousMessages collect: [:s | s truncateWithElipsisTo: 30].
	list ifEmpty: [UIManager default inform: 'No previous log message was entered'. ^ self].
	index := UIManager default chooseFrom: list.
	
	"no comment was selected"
	index isZero ifTrue: [ ^ self ].
	
	self logMessage: (self previousMessages at: index)
]

{ #category : #'log message history' }
MCSaveVersionDialog >> previousMessages [

	^ self class previousMessages
]

{ #category : #'morphic ui' }
MCSaveVersionDialog >> setDefaultFocus [
	(self findTextMorph: #logMessage)
		textArea selectAll;
		takeKeyboardFocus
]

{ #category : #'morphic ui' }
MCSaveVersionDialog >> textMorph: aMorph [
	"here the text field is not used for code editing"
 	^(super textMorph: aMorph) beForPlainText

]

{ #category : #accessing }
MCSaveVersionDialog >> versionName [
	^ name
]

{ #category : #accessing }
MCSaveVersionDialog >> versionName: aString [
	name := aString.
	self changed: #versionName
]

{ #category : #'morphic ui' }
MCSaveVersionDialog >> widgetSpecs [
	^ #(	
		((textMorph: versionName) (0 0 1 0) (0 0 0 30))
		((textMorph: logMessage) (0 0 1 1) (0 30 0 -30))
		((buttonRow) (0 1 1 1) (0 -40 0 0))
		)
]
