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

{ #category : #testing }
PlatformIndependentEnvironment class >> isDefaultFor: aPlatform [ 
	^ false
]

{ #category : #accessing }
PlatformIndependentEnvironment >> at: aKey ifAbsent: aBlock [

	^ nil
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
