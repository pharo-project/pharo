Class {
	#name : 'UserOfPropertyTest',
	#superclass : 'TestCase',
	#traits : 'TPropertiesTest',
	#classTraits : 'TPropertiesTest classTrait',
	#instVars : [
		'keyAccess',
		'removeKey'
	],
	#category : 'Kernel-Traits-Tests',
	#package : 'Kernel-Traits-Tests'
}

{ #category : 'requirements' }
UserOfPropertyTest >> classWithProperties [
	| class |

	class := UserOfProperty new.
	^ class
		propertyAt: #a put: 'a';
		propertyAt: #b put: 42;
		propertyAt: #c put: 'a'; 
		propertyAt: #d put: -2; 
		yourself. 
]

{ #category : 'requirements' }
UserOfPropertyTest >> keyNotIn [ 
	^#z
]

{ #category : 'requirements' }
UserOfPropertyTest >> valueNotIn [ 
	^700
]
