"
I'm a platform independent environment who is intent to work as fallback when none other works. 
I provide stubs for method calls that answers just default values. 

"
Class {
	#name : 'NullOSEnvironment',
	#superclass : 'OSEnvironment',
	#category : 'System-OSEnvironments-Platforms',
	#package : 'System-OSEnvironments',
	#tag : 'Platforms'
}

{ #category : 'testing' }
NullOSEnvironment class >> isAvailable [

	^ true
]

{ #category : 'accessing' }
NullOSEnvironment >> at: aKey ifAbsent: aBlock [
	"Gets the value of an environment variable called `aKey`. Execute aBlock if absent.
	As there is nowhere to look up the key just evaluate the given block"

	^ aBlock value
]

{ #category : 'accessing' }
NullOSEnvironment >> at: aKey put: aValue [
	"Do nothing"
]

{ #category : 'enumeration' }
NullOSEnvironment >> keysAndValuesDo: aBlock [
	"Do nothing"
]

{ #category : 'accessing' }
NullOSEnvironment >> removeKey: key [
	"Do nothing"
]

{ #category : 'accessing' }
NullOSEnvironment >> setEnv: nameString value: valueString [
	"We do not support environment Variables. Do Nothing"
]
