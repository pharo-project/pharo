Class {
	#name : 'RGLayout',
	#superclass : 'RGObject',
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'description' }
RGLayout class >> subclassDefiningSymbol [
	"Answer a keyword that describes the receiver's kind of subclass
	Note: this is for printing the ST80 style class definiton."

	^ #subclass:
]

{ #category : 'managing container' }
RGLayout >> addoptToParentStub [

	super addoptToParentStub.
	self environment backend createUnresolvedClassGroupFor: self parent.
	self parent behaviorStrategy pvtLayout: self
]

{ #category : 'accessing' }
RGLayout >> allSlots [

	^ {  }
]

{ #category : 'managing container' }
RGLayout >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : 'accessing' }
RGLayout >> instVarNames [
	^ {}
]

{ #category : 'testing - types' }
RGLayout >> isBitsLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isByteLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isCompiledMethodLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isEmptyLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isEphemeronLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isFixedLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isImmediateLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isLayout [

	^true
]

{ #category : 'testing - types' }
RGLayout >> isObjectLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isPointerLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isVariableLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isWeakLayout [

	^ false
]

{ #category : 'testing - types' }
RGLayout >> isWordLayout [

	^ false
]

{ #category : 'accessing' }
RGLayout >> layoutName [
	^ (self class name allButFirst: 2) asSymbol
]

{ #category : 'accessing' }
RGLayout >> resolveSlot: aName ifFound: foundBlock ifNone: exceptionBlock [
	^exceptionBlock value
]
