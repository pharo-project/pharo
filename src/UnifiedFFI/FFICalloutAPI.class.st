"
I'm a façade to perform ffi nb calls.
"
Class {
	#name : #FFICalloutAPI,
	#superclass : #Object,
	#instVars : [
		'context',
		'options',
		'callingConvention'
	],
	#classVars : [
		'CalloutAPIClass'
	],
	#category : #UnifiedFFI
}

{ #category : #accessing }
FFICalloutAPI class >> calloutAPIClass [
	^ CalloutAPIClass ifNil: [ CalloutAPIClass := self defaultCalloutAPIClass ]
	
]

{ #category : #accessing }
FFICalloutAPI class >> calloutAPIClass: aClass [
	CalloutAPIClass := aClass
]

{ #category : #accessing }
FFICalloutAPI class >> defaultCalloutAPIClass [
	^ FFICalloutAPI	
]

{ #category : #'instance creation' }
FFICalloutAPI class >> inContext: aContext [
	^ self basicNew 
		initializeContext: aContext;
		yourself
]

{ #category : #'instance creation' }
FFICalloutAPI class >> new [ 
	self error: 'Use #inContext'
]

{ #category : #private }
FFICalloutAPI >> callingConvention [
	^ callingConvention
]

{ #category : #'call conventions' }
FFICalloutAPI >> cdecl [
	callingConvention := #cdecl
]

{ #category : #accessing }
FFICalloutAPI >> context [
	^ context
]

{ #category : #accessing }
FFICalloutAPI >> convention: aCallingConvention [
	"It can be #cdecl or #stdcall"
	callingConvention := aCallingConvention
]

{ #category : #action }
FFICalloutAPI >> function: functionSignature module: moduleNameOrLibrary [
	| sender ffiMethod |
	sender := context ifNil: [ thisContext sender sender ].

	"Build new method"
	ffiMethod := self newBuilder
		build: [ :builder | 
			builder
				signature: functionSignature;
				sender: sender;
				module: moduleNameOrLibrary ].
	ffiMethod
		selector: sender selector;
		methodClass: sender methodClass.
	"Replace with generated ffi method, but save old one for future use"
	ffiMethod propertyValueAt: #ffiNonCompiledMethod put: (sender methodClass methodDict at: sender selector).
	"For senders search, one need to keep the selector in the properties"
	ffiMethod propertyValueAt: #ffiMethodSelector put: sender method sendNodes first selector.
	sender methodClass methodDict at: sender selector put: ffiMethod.
	"Register current method as compiled for ffi"
	FFIMethodRegistry uniqueInstance registerMethod: ffiMethod.
	"Resend"
	sender return: (sender receiver withArgs: sender arguments executeMethod: ffiMethod)
]

{ #category : #initialization }
FFICalloutAPI >> initialize [
	callingConvention := #cdecl.
	options := #().
	super initialize
]

{ #category : #initialization }
FFICalloutAPI >> initializeContext: aContext [
	context := aContext.
	self initialize
]

{ #category : #action }
FFICalloutAPI >> newBuilder [
	^ FFICalloutMethodBuilder calloutAPI: self
]

{ #category : #accessing }
FFICalloutAPI >> options [
	^ options
]

{ #category : #accessing }
FFICalloutAPI >> options: anObject [
	options := anObject
]

{ #category : #'call conventions' }
FFICalloutAPI >> stdcall [
	callingConvention := #stdcall
]
