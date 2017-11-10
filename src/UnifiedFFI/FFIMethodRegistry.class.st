"
I keep a  registry of FFI prepared methods. 
Registry is used to know exactly which methods are executing FFI-NB calls and then which ones need to be recompiled when a platform has change.
"
Class {
	#name : #FFIMethodRegistry,
	#superclass : #Object,
	#instVars : [
		'compiledMethods'
	],
	#classInstVars : [
		'uniqueInstance'
	],
	#category : #UnifiedFFI
}

{ #category : #cleanup }
FFIMethodRegistry class >> cleanUp: aggressive [
	"Only delete change sets when being aggressive"
	self uniqueInstance reset.
	uniqueInstance := nil
	
]

{ #category : #'class initialization' }
FFIMethodRegistry class >> initialize [
	SessionManager default
		registerSystemClassNamed: self name
		atPriority: 60
]

{ #category : #'instance creation' }
FFIMethodRegistry class >> new [
	self error: 'Use #uniqueInstance'
]

{ #category : #'system startup' }
FFIMethodRegistry class >> resetAll [
	"I'm reseting each shutdown and startup because like that I can be sure a complete cleanup is done, 
	 because many things can change: 
		- platform change 
		- working directory change (and then libraries not found)
		- indirect functions needs to be recalculated.
	I am not supposed to be performed when image is just saved to avoid possible problem which can happen when we reset all caches and continue to work"
	self uniqueInstance reset.
	FFICallbackFunctionResolution reset.
	FFIExternalStructure allSubclassesDo: #reset.
	uniqueInstance := nil.
]

{ #category : #'system startup' }
FFIMethodRegistry class >> shutDown: quitting [
	quitting ifFalse: [ ^ self ].
	
	self resetAll
]

{ #category : #'system startup' }
FFIMethodRegistry class >> startUp: isImageStarting [
	isImageStarting ifFalse: [ ^ self ].
	uniqueInstance ifNil: [ ^self ].
	
	self resetAll
]

{ #category : #'instance creation' }
FFIMethodRegistry class >> uniqueInstance [
	^ uniqueInstance ifNil: [ uniqueInstance := super new ]
]

{ #category : #accessing }
FFIMethodRegistry >> compiledMethods [
	^ compiledMethods
]

{ #category : #initialization }
FFIMethodRegistry >> initialize [ 
	super initialize.
	compiledMethods := WeakSet new.
	
]

{ #category : #accessing }
FFIMethodRegistry >> registerMethod: aCompiledMethod [
	compiledMethods add: aCompiledMethod.
]

{ #category : #accessing }
FFIMethodRegistry >> reset [
	"FFI compiled methods will keep the old method as a property, making easy to replace it 
	 when changing platforms."
	self compiledMethods do: [ :each |
		each methodClass methodDict 
			at: each selector 
			put: (each propertyValueAt: #ffiNonCompiledMethod)].
	self compiledMethods removeAll
]
