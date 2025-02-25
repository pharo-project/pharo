"
FloatArrays store 32bit IEEE floating point numbers.

For further comments read comments of `WordArray`.
"
Class {
	#name : 'Float32Array',
	#superclass : 'ArrayedCollection',
	#type : 'words',
	#category : 'Collections-Native-Base',
	#package : 'Collections-Native',
	#tag : 'Base'
}

{ #category : 'comparing' }
Float32Array >> = aFloatArray [
	| length |
	<primitive: 'primitiveEqual' module: 'FloatArrayPlugin'>
	aFloatArray class = self class ifFalse: [^ false].
	length := self size.
	length = aFloatArray size ifFalse: [^ false].
	1 to: self size do: [:i | (self at: i)
			= (aFloatArray at: i) ifFalse: [^ false]].
	^ true
]

{ #category : 'adding' }
Float32Array >> addAssignToFloatArray: aFloatArray [

	^ aFloatArray primAddArray: self
]

{ #category : 'converting' }
Float32Array >> asFloatArray [
	^self
]

{ #category : 'accessing' }
Float32Array >> at: index [
	<primitive: 'primitiveAt' module: 'FloatArrayPlugin'>
	^Float fromIEEE32Bit: (self basicAt: index)
]

{ #category : 'accessing' }
Float32Array >> at: index put: value [
	<primitive: 'primitiveAtPut' module: 'FloatArrayPlugin'>
	value isFloat
		ifTrue:[self basicAt: index put: value asIEEE32BitWord]
		ifFalse:[self at: index put: value asFloat].
	^value
]

{ #category : 'accessing' }
Float32Array >> defaultElement [
	"Return the default element of the receiver"
	^0.0
]

{ #category : 'arithmetic' }
Float32Array >> dot: aFloatVector [
	"Primitive. Return the dot product of the receiver and the argument.
	Fail if the argument is not of the same size as the receiver."
       | result |
	<primitive: 'primitiveDotProduct' module: 'FloatArrayPlugin'>
	self size = aFloatVector size ifFalse:[^self error:'Must be equal size'].
	result := 0.0.
	1 to: self size do:[:i|
		result := result + ((self at: i) * (aFloatVector at: i))].
	^result
]

{ #category : 'comparing' }
Float32Array >> hash [
	| result |
	<primitive:'primitiveHashArray' module: 'FloatArrayPlugin'>
	result := 0.
	1 to: self size do:[:i| result := result + (self basicAt: i) ].
	^result bitAnd: 16r1FFFFFFF
]

{ #category : 'primitives - plugin' }
Float32Array >> primAddArray: floatArray [

	<primitive: 'primitiveAddFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) + (floatArray at: i)]
]

{ #category : 'primitives - plugin' }
Float32Array >> primAddScalar: scalarValue [

	<primitive: 'primitiveAddScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) + scalarValue]
]

{ #category : 'primitives - plugin' }
Float32Array >> primDivArray: floatArray [

	<primitive: 'primitiveDivFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) / (floatArray at: i)]
]

{ #category : 'primitives - plugin' }
Float32Array >> primDivScalar: scalarValue [

	<primitive: 'primitiveDivScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) / scalarValue]
]

{ #category : 'primitives - plugin' }
Float32Array >> primMulArray: floatArray [

	<primitive: 'primitiveMulFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * (floatArray at: i)]
]

{ #category : 'primitives - plugin' }
Float32Array >> primMulScalar: scalarValue [

	<primitive: 'primitiveMulScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) * scalarValue]
]

{ #category : 'primitives - plugin' }
Float32Array >> primSubArray: floatArray [

	<primitive: 'primitiveSubFloatArray' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) - (floatArray at: i)]
]

{ #category : 'primitives - plugin' }
Float32Array >> primSubScalar: scalarValue [

	<primitive: 'primitiveSubScalar' module: 'FloatArrayPlugin'>
	1 to: self size do:[:i| self at: i put: (self at: i) - scalarValue]
]

{ #category : 'private' }
Float32Array >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	"Primitive. This destructively replaces elements from start to stop in the receiver starting at index, repStart, in the collection, replacement. Answer the receiver. Range checks are performed in the primitive only. Optional. See Object documentation whatIsAPrimitive."
	<primitive: 105>
	super replaceFrom: start to: stop with: replacement startingAt: repStart
]

{ #category : 'primitives - plugin' }
Float32Array >> sum [

	<primitive: 'primitiveSum' module: 'FloatArrayPlugin'>
	^ super sum
]
