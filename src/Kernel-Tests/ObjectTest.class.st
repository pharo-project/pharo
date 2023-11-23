"
SUnit tests for objects
"
Class {
	#name : 'ObjectTest',
	#superclass : 'ClassTestCase',
	#category : 'Kernel-Tests-Objects',
	#package : 'Kernel-Tests',
	#tag : 'Objects'
}

{ #category : 'coverage' }
ObjectTest >> classToBeTested [

	^ Object
]

{ #category : 'tests - reflecting' }
ObjectTest >> testAdoptInstance [

	| point |
	self assert: Point format equals: Association format. "non-indexable with 2 ivars"

	point := 10 @ 20.
	self assert: point class equals: Point.
	self assert: point printString equals: '(10@20)'.

	Association adoptInstance: point.
	self assert: point class equals: Association.
	self assert: point printString equals: '10->20'.

	Point adoptInstance: point.
	self assert: point class equals: Point.
	self assert: point printString equals: '(10@20)'.

	self should: [ Object adoptInstance: point ] raise: PrimitiveFailed
]

{ #category : 'tests' }
ObjectTest >> testAs [
	| coll1 coll2 |
	coll1 := {1 . 2 . 3}.
	coll2 := coll1 as: OrderedCollection.

	self assert: coll2 equals: (OrderedCollection with: 1 with: 2 with: 3).
	self deny: coll1 identicalTo: coll2.

	"If the object has the right type, do nothing."
	coll2 := coll1 as: Array.
	self assert: coll1 identicalTo: coll2
]

{ #category : 'tests' }
ObjectTest >> testAssert [

	self shouldnt: [Object new assert: true] raise: AssertionFailure.
	self shouldnt: [
		"make sure that we non-boolean arguments just fails"
		self should: [Object new assert: 1] raise: AssertionFailure ] raise: [NonBooleanReceiver]. 
]

{ #category : 'tests - accessing' }
ObjectTest >> testBasicSize [
	"Numbers are not indexable"
	self assert: 1 basicSize equals: 0.

	"Collections are indexable"
	self assert: #(a b c) basicSize equals: 3
]

{ #category : 'tests - accessing' }
ObjectTest >> testBasicSizeNotOverwritten [
	"#basicSize should not be overwritten by subclasses of Object, except in Context"

	self assert: #basicSize implementors asArray equals: {Context>>#basicSize. Object>>#basicSize}
]

{ #category : 'tests - write barrier' }
ObjectTest >> testBeReadOnlyLiteral [
	self assert: String new beReadOnlyLiteral isReadOnlyObject.
	self deny: Object new beReadOnlyLiteral isReadOnlyObject
]

{ #category : 'tests - write barrier' }
ObjectTest >> testBeRecursivelyReadOnlyObject [

	| assoc array |
	assoc := 1 -> (1 -> 2).
	assoc beRecursivelyReadOnlyObject.
	self should: [ assoc key: 2 ] raise: ModificationForbidden.
	self should: [ assoc value key: 2 ] raise: ModificationForbidden.

	array := Array with: 1 with: (Array with:1 with: 2).
	array beRecursivelyReadOnlyObject.
	self should: [ array at: 1 put: 2 ] raise: ModificationForbidden.
	self should: [ array second  at: 1 put: 2 ] raise: ModificationForbidden
]

{ #category : 'tests - write barrier' }
ObjectTest >> testBeRecursivelyWritableObject [

	| assoc array |
	assoc := 1 -> (1 -> 2).
	assoc beRecursivelyReadOnlyObject.
	self should: [ assoc key: 2 ] raise: ModificationForbidden.
	self should: [ assoc value key: 2 ] raise: ModificationForbidden.
	assoc beRecursivelyWritableObject.
	assoc key: 2.
	assoc value key: 2.
	self assert: assoc key equals: 2.
	self assert: assoc value key equals: 2.

	array := Array with: 1 with: (Array with:1 with: 2).
	array beRecursivelyReadOnlyObject.
	self should: [ array at: 1 put: 2 ] raise: ModificationForbidden.
	self should: [ array second  at: 1 put: 2 ] raise: ModificationForbidden.
	array beRecursivelyWritableObject.
	array at: 1 put: 2.
	array second at: 1 put: 2.
	self assert: array first equals: 2.
	self assert: array second first equals: 2
]

{ #category : 'tests - reflecting' }
ObjectTest >> testBecome [
	"this test should that all the variables pointing to an object are pointing now to another one, and all
      object pointing to the other are pointing to the object"

	| pt1 pt2 pt3 |
	pt1 := 0 @ 0.
	pt2 := pt1.
	pt3 := 100 @ 100.

	pt1 become: pt3.
	self assert: pt2 equals: 100 @ 100.
	self assert: pt3 equals: 0 @ 0.
	self assert: pt1 equals: 100 @ 100
]

{ #category : 'tests - reflecting' }
ObjectTest >> testBecomeForward [
	"this test should that all the variables pointing to an object are pointing now to another one.
	Not that this inverse is not true. This kind of become is called oneWayBecome in VW"

	| pt1 pt2 pt3 |
	pt1 := 0 @ 0.
	pt2 := pt1.
	pt3 := 100 @ 100.
	pt1 becomeForward: pt3.
	self assert: pt2 equals: 100 @ 100.
	self assert: pt3 identicalTo: pt2.
	self assert: pt1 equals: 100 @ 100
]

{ #category : 'tests' }
ObjectTest >> testCaseOf [
	| temp |

	"Test for Value"
	temp := #case1 caseOf: {
 		[#case1]->[ 1 ].
  		[#case2]->[ 2 ] }.
 	self assert: temp equals: 1.

  	temp := #case2 caseOf: {
 		[#case1]->[ 1 ].
  		[#case2]->[ 2 ] }.
 	self assert: temp equals: 2.

  	self should: [
 		temp := #case3 caseOf: {
 			[#case1]->[ 1 ].
  			[#case2]->[ 2 ] }
 		] raise: Error.

	"Test for effect"
	#case1 caseOf: {
		[#case1]->[ temp := 1 ].
 		[#case2]->[ temp := 2 ] }.
	self assert: temp equals: 1.

	#case2 caseOf: {
		[#case1]->[ temp := 1 ].
 		[#case2]->[ temp := 2 ] }.
	self assert: temp equals: 2.

	self should: [
		#case3 caseOf: {
			[#case1]->[ 1 ].
 			[#case2]->[ 2 ] }
		] raise: Error
]

{ #category : 'tests' }
ObjectTest >> testCaseOfOtherwise [
	| temp |
	"Test for Value"
	temp := #case1
		caseOf: {
			[#case1]->[ 1 ].
 			[#case2]->[ 2 ] }
		otherwise: [ 3 ].
	self assert: temp equals: 1.

	temp := #case2
		caseOf: {
			[#case1]->[ 1 ].
 			[#case2]->[ 2 ] }
		otherwise: [ 3 ].
	self assert: temp equals: 2.

	temp := #case3
		caseOf: {
			[#case1]->[ 1 ].
 			[#case2]->[ 2 ] }
		otherwise: [ 3 ].
	self assert: temp equals: 3.

	"Disabled due to bootrap not using Pharo9
	temp := #case4
		caseOf: { }
		otherwise: [ 3 ].
	self assert: temp equals: 3."

	"Test Effect"
	#case1
		caseOf: {
			[#case1]->[ temp := 1 ].
 			[#case2]->[ temp := 2 ] }
		otherwise: [ temp := 3 ].
	self assert: temp equals: 1.

	#case2
		caseOf: {
			[#case1]->[ temp := 1 ].
 			[#case2]->[ temp := 2 ] }
		otherwise: [ temp := 3 ].
	self assert: temp equals: 2.

	#case3
		caseOf: {
			[#case1]->[ temp := 1 ].
 			[#case2]->[ temp := 2 ] }
		otherwise: [ temp := 3 ].
	self assert: temp equals: 3
]

{ #category : 'tests - printing' }
ObjectTest >> testDisplayString [

	self assert: Object new displayString equals: 'an Object'
]

{ #category : 'tests - printing' }
ObjectTest >> testDisplayStringLimitedString [

	| actual |
	actual := Object new displayStringLimitedTo: 8.
	self assert: actual equals: 'an O[..]'
]

{ #category : 'tests' }
ObjectTest >> testInstVarNamed [
	| obj |
	obj := ObjectMockForTest new.
	self assert: (obj instVarNamed: 'variable') isNil.
	obj variable: 1.
	self assert: (obj instVarNamed: 'variable') equals: 1.
	self shouldnt: [ obj instVarNamed: 'variable' ] raise: InstanceVariableNotFound.
	self should: [ obj instVarNamed: 'timoleon' ] raise: InstanceVariableNotFound.
	self
		assert:
			([ obj instVarNamed: 'timoleon' ]
				on: InstanceVariableNotFound
				do: [ :ex | ex instVarName ])
		equals: 'timoleon'
]

{ #category : 'tests' }
ObjectTest >> testInstVarNamedPut [
	| obj |
	obj := ObjectMockForTest new.
	self assert: (obj instVarNamed: 'variable') isNil.
	obj instVarNamed: 'variable' put: 1.
	self assert: (obj instVarNamed: 'variable') equals: 1.
	self shouldnt: [ obj instVarNamed: 'variable' put: 1 ] raise: InstanceVariableNotFound.
	self should: [ obj instVarNamed: 'timoleon' put: 1 ] raise: InstanceVariableNotFound
]

{ #category : 'tests - reflecting' }
ObjectTest >> testPrimitiveChangeClassTo [

	| point |
	self assert: Point format equals: Association format. "non-indexable with 2 ivars"

	point := 10 @ 20.
	self assert: point class equals: Point.
	self assert: point printString equals: '(10@20)'.

	point primitiveChangeClassTo: Association new.
	self assert: point class equals: Association.
	self assert: point printString equals: '10->20'.

	point primitiveChangeClassTo: Point new.
	self assert: point class equals: Point.
	self assert: point printString equals: '(10@20)'.

	self should: [ point primitiveChangeClassTo: Object new ] raise: PrimitiveFailed
]

{ #category : 'tests - printing' }
ObjectTest >> testPrintLimitedString [

	| actual |
	actual := Object new printStringLimitedTo: 8.
	self assert: actual equals: 'an O[..]'
]

{ #category : 'tests - printing' }
ObjectTest >> testPrintString [

	self assert: Object new printString equals: 'an Object'
]

{ #category : 'tests - introspection' }
ObjectTest >> testReadSlot [
	self assert: (5 @ 3 readSlot: (Point slotNamed: #x)) equals: 5
]

{ #category : 'tests - introspection' }
ObjectTest >> testReadSlotNamed [
	self assert: (5 @ 3 readSlotNamed: #x) equals: 5
]

{ #category : 'tests - introspection' }
ObjectTest >> testWriteSlotNamedValue [
	| object |
	object := 5 @ 6.

	self assert: (object writeSlotNamed: #x value: 7) equals: 7.
	self assert: object equals: 7 @ 6
]

{ #category : 'tests - introspection' }
ObjectTest >> testWriteSlotValue [
	| object |
	object := 5 @ 6.

	self assert: (object writeSlot: (Point slotNamed: #x) value: 7) equals: 7.
	self assert: object equals: 7 @ 6
]
