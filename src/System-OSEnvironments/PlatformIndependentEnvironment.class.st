"
I'm a platform independent environment who is intent to work when none other works. 
Specially, I'm intended to work as a replacement for environments when there is no NativeBoost present, therefore I provide stubs for method calls that answers just default values. 

"
Class {
	#name : #PlatformIndependentEnvironment,
	#superclass : #OSEnvironment,
	#category : #'System-OSEnvironments-Platforms'
}

{ #category : #testing }
PlatformIndependentEnvironment class >> isAvailable [ 
	^ true
]

{ #category : #accessing }
PlatformIndependentEnvironment >> at: aKey ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`. Execute aBlock if absent. 
	As there is nowhere to look up the key just evaluate the given block"

	^ aBlock value
]

{ #category : #accessing }
PlatformIndependentEnvironment >> at: aKey put: aValue [
	"Do nothing"
]

{ #category : #enumeration }
PlatformIndependentEnvironment >> keysAndValuesDo: aBlock [
	"Do nothing"
]

{ #category : #accessing }
PlatformIndependentEnvironment >> removeKey: key [
	"Do nothing"
]

{ #category : #accessing }
PlatformIndependentEnvironment >> setEnv: nameString value: valueString [
	"We do not support environment Variables. Do Nothing"
]
