"
I am a debug point which keeps track of the values of the node I am attached to. Whenever the debug point is hit, the corresponding value is recorded in my history. It is possible to set a limit to the value history
"
Class {
	#name : 'WatchDebugPoint',
	#superclass : 'DebugPoint',
	#instVars : [
		'history',
		'limit'
	],
	#category : 'DebugPoints-Implementations',
	#package : 'DebugPoints',
	#tag : 'Implementations'
}

{ #category : 'API' }
WatchDebugPoint >> addValue: aValue [

	self history addFirst: aValue.
	self limit = 0 ifFalse: [
		self history size > self limit ifTrue: [ self history removeLast ] ]
]

{ #category : 'default values' }
WatchDebugPoint >> args [

	^ #( context value )
]

{ #category : 'default values' }
WatchDebugPoint >> control [

	^ #after
]

{ #category : 'accessing' }
WatchDebugPoint >> history [

	^ history ifNil: [ history := OrderedCollection new ]
]

{ #category : 'accessing' }
WatchDebugPoint >> history: anOrderedCollection [

	history := anOrderedCollection
]

{ #category : 'API' }
WatchDebugPoint >> hitWithContext: aContext value: aValue [

	(self hitWithContext: aContext) ifFalse: [ ^ self ].
	self addValue: aValue.
	DebugPointManager notifyDebugPointChanged: self
]

{ #category : 'accessing' }
WatchDebugPoint >> limit [

	^ limit ifNil: [ limit := 0 ]
]

{ #category : 'accessing' }
WatchDebugPoint >> limit: anInteger [

	limit := anInteger.
	anInteger = 0 ifFalse: [
		self history size > anInteger ifTrue: [
			self history: (self history copyFrom: 1 to: anInteger) ] ]
]

{ #category : 'default values' }
WatchDebugPoint >> metaLink [

	^ super metaLink
		  option: #( #+ optionWeakAfter #+ optionCompileOnLinkInstallation );
		  yourself
]

{ #category : 'accessing' }
WatchDebugPoint >> name [

	^ name ifNil: [ #WatchPoint ]
]

{ #category : 'default values' }
WatchDebugPoint >> selector [

	^ #hitWithContext:value:
]

{ #category : 'accessing' }
WatchDebugPoint >> type [

	^ #Watchpoint
]
