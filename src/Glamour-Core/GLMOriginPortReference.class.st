"
Used by BundleTransmission to annotate its originating ports whether they are active or passive.

When a BundleTransmission consideres a port as active, the browser will trigger it when the corresponding port changes.
"
Class {
	#name : #GLMOriginPortReference,
	#superclass : #GLMPortReference,
	#instVars : [
		'active',
		'transformation'
	],
	#category : #'Glamour-Core'
}

{ #category : #accessing }
GLMOriginPortReference >> beActive [ 
	 
	active := true
]

{ #category : #accessing }
GLMOriginPortReference >> bePassive [ 
	 
	active := false
]

{ #category : #testing }
GLMOriginPortReference >> isActive [ 
	 
	^active ifNil: [active := true]
]

{ #category : #testing }
GLMOriginPortReference >> isPassive [ 
	 
	^self isActive not
]

{ #category : #accessing }
GLMOriginPortReference >> transformation [
	^ transformation
]

{ #category : #accessing }
GLMOriginPortReference >> transformation: anObject [
	transformation := anObject
]

{ #category : #'accessing-convenience' }
GLMOriginPortReference >> value [

	^ self transformation
		ifNil: [ self port value ]
		ifNotNil: [ self transformation value: self port value ]
]
