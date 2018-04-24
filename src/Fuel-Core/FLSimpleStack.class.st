"
FLSimpleStack is a simple stack used in Fuel to avoid a recursion while traversing the graph to serialize. While analizing the content of an object (inst vars) we can encounter simple objects (direct string representation) or complex objects (composite). In the latter case we start analizing those instVars.

So... Imagine object X with two instVars Y and Z. Imagine the method of the traverse is called #analye:.  So you do #analize: X.  Then, in such method you check whether X has regular pointers to regular objects, and if true, you analize them. So in this case you would send #analyze: Y and #analize: Z, generating the recursion. Right ?  Notice that the time between analyse: Y and analyse: Z is dependent on the graph that is attached to Y. Y can have objects that have further objects etc. So leaving X aside, Y can be the first object to serialize and Z the last.

With this FLSimpleStack, instead of sending #analyze: Y and #analize: Z  what Fuel does is just to do a push on a stack:   #push: Y  and #push: Z. And then we are done with X, so we pop from the stack and we continue with the next object (at some point in the future we will pop Y and Z) When there are no more objects in the stack it means we are done. 
"
Class {
	#name : #FLSimpleStack,
	#superclass : #Object,
	#instVars : [
		'array',
		'slotIndex'
	],
	#category : #'Fuel-Core-Collections'
}

{ #category : #'instance creation' }
FLSimpleStack class >> hintSize [

	^64
]

{ #category : #'instance creation' }
FLSimpleStack class >> new [
	
	^ self new: self hintSize
]

{ #category : #'instance creation' }
FLSimpleStack class >> new: nElements [
	"Create a Set large enough to hold nElements without growing"
	^ self basicNew initialize: nElements
]

{ #category : #accessing }
FLSimpleStack >> capacity [

	^ array size
]

{ #category : #adding }
FLSimpleStack >> grow [
	"Become larger in capacity."
	| newArray |
	newArray := Array new: array size * 2.
	newArray replaceFrom: 1 to: array size with: array startingAt: 1.
	array := newArray
]

{ #category : #'initialize-release' }
FLSimpleStack >> initialize: hintSize [

	array := Array new: hintSize.
	slotIndex := 1.
]

{ #category : #accessing }
FLSimpleStack >> isEmpty [

	^ 1 = slotIndex
]

{ #category : #removing }
FLSimpleStack >> pop [
	"Returns the first element and remove it from the stack."

	slotIndex := slotIndex - 1.	
	^ array at: slotIndex 
]

{ #category : #printing }
FLSimpleStack >> printOn: aStream [ 
	"Append a sequence of characters that identify the receiver to aStream."

	super printOn: aStream.
	array printElementsOn: aStream

]

{ #category : #adding }
FLSimpleStack >> push: anObject [ 
	"Adds a new object of any kind on top of the stack."

	array at: slotIndex put: anObject.
	array size = slotIndex ifTrue: [ self grow ].
	slotIndex := slotIndex + 1.
	^ anObject.
]
