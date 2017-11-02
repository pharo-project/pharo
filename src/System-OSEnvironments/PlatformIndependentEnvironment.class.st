"
I'm a platform independent environment who is intent to work when none other works. 
Specially, I'm intended to work as a replacement for environments when there is no NativeBoost present, therefore I provide stubs for method calls that answers just default values. 

"
Class {
	#name : #PlatformIndependentEnvironment,
	#superclass : #OSEnvironment,
	#category : 'System-OSEnvironments'
}

{ #category : #testing }
PlatformIndependentEnvironment class >> isAvailable [ 
	^ true
]

{ #category : #testing }
PlatformIndependentEnvironment class >> isDefaultFor: aPlatform [ 
	^ false
]

{ #category : #private }
PlatformIndependentEnvironment >> getEnv: aVariableName [
	^ nil
]

{ #category : #enumeration }
PlatformIndependentEnvironment >> keysAndValuesDo: aBlock [
	"Do nothing"
]

{ #category : #private }
PlatformIndependentEnvironment >> setEnv: nameString value: valueString [
	"Do nothing"
]

{ #category : #private }
PlatformIndependentEnvironment >> unsetEnv: aString [
	^ self setEnv: aString value: nil
]
