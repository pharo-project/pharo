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
	"Gets the value of an environment variable called `aKey`.
	Execute aBlock if absent.
	It is the system reponsibility to manage the encoding.
	
	Rationale: A common denominator for all platforms providing an already decoded string, because windows does not (compared to *nix systems) provide a encoded byte representation of the value. Windows has instead its own wide string representation."
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
