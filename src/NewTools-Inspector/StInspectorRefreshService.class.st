"
I'm a service to process refresh of inspectors. 
I keep a registry of all opened inspectors and execute the required stepping (by default every 500ms), but I'm only active if I have inspectors registered (otherwise, I will stop until a new inspector is registered) 
"
Class {
	#name : #StInspectorRefreshService,
	#superclass : #TKTService,
	#instVars : [
		'inspectors',
		'mutex'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #'NewTools-Inspector-View'
}

{ #category : #'instance creation' }
StInspectorRefreshService class >> new [

	self error: 'Use #uniqueInstance'
]

{ #category : #'instance creation' }
StInspectorRefreshService class >> uniqueInstance [

	^ uniqueInstance ifNil: [ uniqueInstance := self basicNew initialize ]
]

{ #category : #initialization }
StInspectorRefreshService >> initialize [

	super initialize.
	self stepDelay: 1000 milliSeconds.
	mutex := Mutex new.
	inspectors := Set new
]

{ #category : #accessing }
StInspectorRefreshService >> name [
	
	^ 'StInspector Refresh Service'
]

{ #category : #registry }
StInspectorRefreshService >> register: anInspector [

	mutex critical: [ 
		inspectors add: anInspector.
		self isRunning ifFalse: [ 
			self start ] ].
]

{ #category : #stepping }
StInspectorRefreshService >> stepService [

	inspectors do: [ :each | each step ]
]

{ #category : #registry }
StInspectorRefreshService >> unregister: anInspector [

	mutex critical: [ 
		inspectors remove: anInspector ifAbsent: [ ].
		inspectors ifEmpty: [ 
			self stop ] ]
]
