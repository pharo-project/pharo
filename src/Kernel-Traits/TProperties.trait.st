"
I represent the properties of a Object. 
My public API is : 

properties
propertyAt: 
propertyAt:put: 
propertyAt:ifPresent: 
propoertyAt:ifAbsent:
propoertyAt:ifAbsentPut:
propertyAt:fPresent:ifAbsent:
propertyAt:ifPresent:fAbsentPut:
removeProperty: 
removeProperty:ifAbsent:

My implementors should implement
"
Trait {
	#name : 'TProperties',
	#instVars : [
		'properties'
	],
	#category : 'Kernel-Traits-Base',
	#package : 'Kernel-Traits',
	#tag : 'Base'
}

{ #category : 'accessing' }
TProperties >> properties [
	^ properties ifNil: [ properties := SmallDictionary new ]
]

{ #category : 'accessing' }
TProperties >> propertyAt: propertyKey [

	^ self properties at: propertyKey 
]

{ #category : 'accessing' }
TProperties >> propertyAt: propertyKey ifAbsent: aBlock [

	^ self properties at: propertyKey ifAbsent: aBlock
]

{ #category : 'accessing' }
TProperties >> propertyAt: propertyKey ifAbsentPut: absentBlock [

	^ self properties at: propertyKey ifAbsentPut: absentBlock 
]

{ #category : 'accessing' }
TProperties >> propertyAt: propertyKey ifPresent: aBlock [

	^ self properties at: propertyKey ifPresent: aBlock
]

{ #category : 'accessing' }
TProperties >> propertyAt: propertyKey ifPresent: presentBlock ifAbsent: absentBlock [
	^ self properties at: propertyKey ifPresent: presentBlock ifAbsent: absentBlock
]

{ #category : 'accessing' }
TProperties >> propertyAt: propertyKey ifPresent: presentBlock ifAbsentPut: absentBlock [
	^ self properties at: propertyKey ifPresent: presentBlock ifAbsentPut: absentBlock
]

{ #category : 'accessing' }
TProperties >> propertyAt: propertyKey put: propertyValue [

	^ self properties at: propertyKey put: propertyValue 
]

{ #category : 'accessing' }
TProperties >> removeProperty: propertyKey [ 
	^self properties removeKey: propertyKey 
]
