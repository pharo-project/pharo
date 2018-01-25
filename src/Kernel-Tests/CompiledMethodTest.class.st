"
This is the unit test for the class CompiledMethod. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : #CompiledMethodTest,
	#superclass : #ClassTestCase,
	#instVars : [
		'x',
		'y'
	],
	#category : #'Kernel-Tests-Methods'
}

{ #category : #'tests - performing' }
CompiledMethodTest >> a1: a1 a2: a2 a3: a3 a4: a4 a5: a5 a6: a6 a7: a7 a8: a8 a9: a9 a10: a10 a11: a11 a12: a12 a13: a13 a14: a14 a15: a15 [
	"I'm a method with the maximum size of arguments that can be executed via normal send but crash on perform :)"
	
	^ a1 + a2 - a2
]

{ #category : #examples }
CompiledMethodTest >> abstractMethod [
	"I am an abstract method"
	
	^ self subclassResponsibility
]

{ #category : #'private - accessing' }
CompiledMethodTest >> categoryNameForTemporaryClasses [
	"Answer the category where to classify temporarily created classes"

	^'Dummy-Tests-Class'
]

{ #category : #coverage }
CompiledMethodTest >> classToBeTested [
	
	^CompiledMethod
]

{ #category : #examples }
CompiledMethodTest >> deprecatedMethod [
	"Used to test sends of deprecation messages;
	do not recategorize in one of the 'deprecated' categories."

	self deprecated: 'example of a deprecated method'
]

{ #category : #examples }
CompiledMethodTest >> deprecatedMethod2 [
	"Used to test sends of deprecation messages;
	do not recategorize in one of the 'deprecated' categories."

	self deprecated: 'example of a deprecated method' on: 'date' in: 'someversion'.
]

{ #category : #examples }
CompiledMethodTest >> deprecatedMethod3 [
	"Used to test sends of deprecation messages;
	do not recategorize in one of the 'deprecated' categories."

	self
		deprecated: 'Example of a deprecated method with transform'
		transformWith: '`@receiver deprecatedMethod3' 
						-> '`@receiver deprecatedMethod3'.

]

{ #category : #examples }
CompiledMethodTest >> deprecatedMethod4 [
	"Used to test sends of deprecation messages;
	do not recategorize in one of the 'deprecated' categories."

	self
	
		deprecated: 'Example of a deprecated method with transform'
		on: '01/01/1970'
		in: #Pharo6
		transformWith: '`@receiver deprecatedMethod4' 
						-> '`@receiver deprecatedMethod4'.

]

{ #category : #deprecated }
CompiledMethodTest >> deprecatedMethod5 [
]

{ #category : #'category - deprecated' }
CompiledMethodTest >> deprecatedMethod6 [
]

{ #category : #'deprecated - category' }
CompiledMethodTest >> deprecatedMethod7 [
]

{ #category : #examples }
CompiledMethodTest >> nonAbstractMethod [
	"I am not an abstract method"
	
	^ 4 + 5
]

{ #category : #examples }
CompiledMethodTest >> readX [
	| tmp |
	tmp := x.
	^ tmp
]

{ #category : #examples }
CompiledMethodTest >> readXandY [
	
	^ x + y
	
]

{ #category : #examples }
CompiledMethodTest >> returnPlusOne: anInteger [
	^anInteger + 1. 
]

{ #category : #examples }
CompiledMethodTest >> returnTrue [
	^true  
]

{ #category : #examples }
CompiledMethodTest >> shouldNotImplementMethod [
	"I am not an abstract method"
	
	^ self shouldNotImplement
]

{ #category : #running }
CompiledMethodTest >> tearDown [
	RPackage organizer unregisterPackageNamed: self categoryNameForTemporaryClasses.
	super tearDown
]

{ #category : #'tests - slots' }
CompiledMethodTest >> testAccessesSlot [

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode) 
		ifFalse: [ ^ self ].
	
	self assert: ((Point>>#x) accessesSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) accessesSlot: (Point slotNamed: #y)).
	self assert: ((Point>>#setX:setY:) accessesSlot: (Point slotNamed: #y)).
	
	
]

{ #category : #'tests - accessing' }
CompiledMethodTest >> testBytecode [
	"The result of this test depends on the used bytecode set. 
	
	 Because there are multiple versions of the encoders currently depending on the compiler used, we test the class name instead of the encoder class itself"
	| method expectedResult |
	method := Object>>#halt.
	expectedResult := (method encoderClass name endsWith: 'SistaV1')
		ifTrue: [#[16 129 216 88]]
		ifFalse: [#[64 209 135 120]].
	self assertCollection: (Object>>#halt) bytecode equals: expectedResult
]

{ #category : #'tests - accessing' }
CompiledMethodTest >> testComparison [
	| method1 method2 |
	method1 := Float class >> #nan.
	method2 := thisContext method.
	
	self assert: method1 = method1.
	self assert: method2 = method2.
	self deny: method1 = method2.
	self deny: method2 = method1.
	
	Object methods do: [ :each |
		self deny: method1 = each.
		self deny: each = method1.
		self deny: method2 = each.
		self deny: each = method2 ]
]

{ #category : #'tests - comparing' }
CompiledMethodTest >> testCopy [
	<pragma: #pragma>
	| method copy |
	method := thisContext method.
	self assert: method pragmas notEmpty.
	copy := method copy.
	self assert: (method equivalentTo: copy).
	self assert: method header = copy header.
	self assert: method = copy.
	self assert: method ~~ copy.
	self assert: copy penultimateLiteral method == copy.
	self assert: method penultimateLiteral method == method.
	method pragmas do:
		[:p|
		self assert: p method == method].
	copy pragmas do:
		[:p|
		self assert: p method == copy]

]

{ #category : #'tests - comparing' }
CompiledMethodTest >> testCopyWithTrailerBytes [
	<pragma: #pragma>
	| method copy |
	method := thisContext method.
	self assert: method pragmas notEmpty.
	copy := method copyWithTrailerBytes: method trailer.
	self assert: (method equivalentTo: copy).
	self deny: method == copy.
	self assert: method symbolic = copy symbolic. "but their bytecode should be the same"
	self assert: method ~~ copy.
	self assert: copy penultimateLiteral method == copy.
	self assert: method penultimateLiteral method == method.
	method pragmas do:
		[:p|
		self assert: p method == method].
	copy pragmas do:
		[:p|
		self assert: p method == copy]

]

{ #category : #'tests - instance variable' }
CompiledMethodTest >> testHasInstVarRef [
		
	| method  |
	method := self class compiledMethodAt: #readX.
	self assert: (method hasInstVarRef).

	method := self class compiledMethodAt: #readXandY.
	self assert: (method hasInstVarRef).
	
	method := self class compiledMethodAt: #writeX.
	self assert: (method hasInstVarRef).
	
	method := self class compiledMethodAt: #writeXandY.
	self assert: (method hasInstVarRef).
	
]

{ #category : #'tests - abstract' }
CompiledMethodTest >> testIsAbstract [

	self assert: (self class >> #abstractMethod) isAbstract.
	self deny: (self class >> #nonAbstractMethod) isAbstract.
	self deny: (self class >> #shouldNotImplementMethod) isAbstract
]

{ #category : #'tests - testing' }
CompiledMethodTest >> testIsDeprecated [
	| deprecatedSelectors |
	deprecatedSelectors := #(deprecatedMethod deprecatedMethod2 deprecatedMethod3 deprecatedMethod4 deprecatedMethod5 deprecatedMethod6 deprecatedMethod7)
		asSet.
	self class
		selectorsDo: [ :each | 
			(deprecatedSelectors includes: each)
				ifTrue: [ self assert: (self class >> each) isDeprecated ]
				ifFalse: [ self deny: (self class >> each) isDeprecated ] ]
]

{ #category : #'tests - testing' }
CompiledMethodTest >> testIsQuick [
	| method  |

	method := self class compiledMethodAt: #returnTrue.
	self assert: (method isQuick).

	method := self class compiledMethodAt: #returnPlusOne:.
	self deny: (method isQuick).

	
]

{ #category : #'tests - accessing' }
CompiledMethodTest >> testOrigin [
	| regularMethod methodFromTrait |

	"Regular method"
	regularMethod := Behavior>>#name.
	
	"Method from a trait without alias "
	methodFromTrait := Behavior>>#addToComposition:.

	self assert: regularMethod origin == regularMethod originMethod methodClass.
	self assert: methodFromTrait origin == methodFromTrait originMethod methodClass.

]

{ #category : #'tests - performing' }
CompiledMethodTest >> testPerformCanExecutelongMethodWithTemps [	
	"the perform: primitive reuses the context of the method calling it. The primitive adds performed selector arguments to the context variables list. So this means that you can execute some methods but not performed them if the calling methods defined too many temps "
	
	| temp1 temp2 temp3 |
	temp1 := 33.
	temp2 := 666.
	temp3 := 42. 
	self assert: (self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15: withArguments: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) = 1.
	self assert: (self class>>#testPerformCanExecutelongMethodWithTemps) frameSize = CompiledMethod smallFrameSize.
	self assert: (self class>>#a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:) frameSize = CompiledMethod fullFrameSize.
]

{ #category : #'tests - performing' }
CompiledMethodTest >> testPerformInSuperclassCanExecutelongMethodWithTemps [	
	"the perform: primitive reuses the context of the method calling it. The primitive adds performed selector arguments to the context variables list. So this means that you can execute some methods but not performed them if the calling methods defined too many temps "
	
	| temp1 temp2 temp3 |
	temp1 := 33.
	temp2 := 666.
	temp3 := 42. 
	self assert: (self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15: withArguments: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) inSuperclass: self class) = 1
]

{ #category : #'tests - instance variable' }
CompiledMethodTest >> testReadsField [		
	| method |
	method := self class compiledMethodAt: #readX.
	self assert: (method readsField: 3).
	
	method := self class compiledMethodAt: #readXandY.
	self assert: (method readsField: 4).
	
	
	"read is not write"
	method := self class compiledMethodAt: #writeX.
	self deny: (method readsField: 3).
	
	method := self class compiledMethodAt: #writeXandY.
	self deny: (method readsField: 3).
	
	method := self class compiledMethodAt: #writeXandY.
	self deny: (method readsField: 4)
]

{ #category : #'tests - slots' }
CompiledMethodTest >> testReadsSlot [

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode) 
		ifFalse: [ ^ self ].

	self assert: ((Point>>#x) readsSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) readsSlot: (Point slotNamed: #y)).
	self deny: ((Point>>#setX:setY:) readsSlot: (Point slotNamed: #y)).
	
	
]

{ #category : #'tests - testing' }
CompiledMethodTest >> testSendsSelector [
	self assert: ((CompiledCode >> #sendsSelector:) sendsSelector: #includes:).
	self deny: ((CompiledCode >> #sendsSelector:) sendsSelector: #doBreakfastForMe)
]

{ #category : #'tests - evaluating' }
CompiledMethodTest >> testValueWithReceiverArguments [
	
	| method value |

	method := self class compiledMethodAt: #returnTrue.

	value := method valueWithReceiver: nil arguments: #().
	self assert: (value = true).

	method := self class compiledMethodAt: #returnPlusOne:.
	value := method valueWithReceiver: nil arguments: #(1).
	self assert: (value = 2).	
]

{ #category : #'tests - instance variable' }
CompiledMethodTest >> testWritesField [	
	| method |
	method := self class compiledMethodAt: #writeX.
	self assert: (method writesField: 3).
	
	method := self class compiledMethodAt: #writeXandY.
	self assert: (method writesField: 3).
	
	method := self class compiledMethodAt: #writeXandY.
	self assert: (method writesField: 4).
	
	"write is not read"
	
	method := self class compiledMethodAt: #readX.
	self deny: (method writesField: 3).
	
	method := self class compiledMethodAt: #readXandY.
	self deny: (method writesField: 3).
	
	method := self class compiledMethodAt: #readXandY.
	self deny: (method writesField: 4).
]

{ #category : #'tests - slots' }
CompiledMethodTest >> testWritesSlot [

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode) 
		ifFalse: [ ^ self ].

	self deny: ((Point>>#x) writesSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) writesSlot: (Point slotNamed: #y)).
	
	self assert: ((Point>>#setX:setY:) writesSlot: (Point slotNamed: #y)).
	self assert: ((Point>>#setX:setY:) writesSlot: (Point slotNamed: #x)).
	
]

{ #category : #examples }
CompiledMethodTest >> writeX [
	
	x := 33
	
]

{ #category : #examples }
CompiledMethodTest >> writeXandY [
	
	x := 33.
	y := 66
	
]
