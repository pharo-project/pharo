"
This is the unit test for the class CompiledMethod. Unit tests are a good way to exercise the functionality of your system in a repeatable and automatic manner. They are therefore recommended if you plan to release anything. For more information, see: 
	- http://www.c2.com/cgi/wiki?UnitTest
	- there is a chapter in the PharoByExample book (http://pharobyexample.org)
	- the sunit class category
"
Class {
	#name : 'CompiledMethodTest',
	#superclass : 'ClassTestCase',
	#instVars : [
		'class',
		'someClass'
	],
	#category : 'Kernel-Extended-Tests-Methods',
	#package : 'Kernel-Extended-Tests',
	#tag : 'Methods'
}

{ #category : 'tests - performing' }
CompiledMethodTest >> a1: a1 a2: a2 a3: a3 a4: a4 a5: a5 a6: a6 a7: a7 a8: a8 a9: a9 a10: a10 a11: a11 a12: a12 a13: a13 a14: a14 a15: a15 [
	"I'm a method with the maximum size of arguments that can be executed via normal send but crash on perform :)"

	^ a1 + a2 - a2
]

{ #category : 'coverage' }
CompiledMethodTest >> classToBeTested [

	^ CompiledMethod
]

{ #category : 'running' }
CompiledMethodTest >> packageNameForTests [
	^ #'Generated-Compiled-Method-Test-Package'
]

{ #category : 'running' }
CompiledMethodTest >> setUp [
	super setUp.
	testingEnvironment := Smalltalk globals.
	someClass := SomeClassForCompiledMethodTests
]

{ #category : 'accessing' }
CompiledMethodTest >> someClass [

	^ someClass 
]

{ #category : 'running' }
CompiledMethodTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTests.
	class ifNotNil: [ class removeFromSystem ].
	
	(self someClass >> #returnTrue) recompile.
	super tearDown
]

{ #category : 'tests - instance variable' }
CompiledMethodTest >> testAccessesField [
	| method |
	method := self someClass compiledMethodAt: #readX.
	self assert: (method accessesField: 4).

	method := self someClass compiledMethodAt: #readXandY.
	self assert: (method accessesField: 5).


	"read is not write"
	method := self someClass compiledMethodAt: #writeX.
	self assert: (method accessesField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self assert: (method accessesField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self assert: (method accessesField: 5)
]

{ #category : 'tests - slots' }
CompiledMethodTest >> testAccessesSlot [

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode)
		ifFalse: [ ^ self ].

	self assert: ((Point>>#x) accessesSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) accessesSlot: (Point slotNamed: #y)).
	self assert: ((Point>>#setX:setY:) accessesSlot: (Point slotNamed: #y))
]

{ #category : 'tests - compilation' }
CompiledMethodTest >> testAddingSlotDoesNotRemoveExtension [
	"Regression test for a case where recompiling a method removed the fact that it was an extension."

	[
	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         name: #TUTU;
			         slots: { #test };
			         package: self packageNameForTests ].
	class compile: 'isExtensionTestMethod ^ test' classified: '*GeneratedPackageForTest'.
	self assert: (class >> #isExtensionTestMethod) isExtension.

	class addInstVarNamed: 'test2'.

	self assert: (class >> #isExtensionTestMethod) isExtension ] ensure: [ self packageOrganizer removePackage: 'GeneratedPackageForTest' ]
]

{ #category : 'tests - converting' }
CompiledMethodTest >> testAsString [

	self assert: thisContext method asString isString
]

{ #category : 'tests - accessing' }
CompiledMethodTest >> testBytecode [
	"The result of this test depends on the used bytecode set.

	 Because there are multiple versions of the encoders currently depending on the compiler used, we test the class name instead of the encoder class itself"
	| method expectedResult |
	method := Object>>#halt.
	expectedResult := (method encoderClass name endsWith: 'SistaV1')
		ifTrue: [#[16 129 216 88]]
		ifFalse: [#[64 209 135 120]].
	self assertCollection: (Object>>#halt) bytecodes equals: expectedResult
]

{ #category : 'tests - copying' }
CompiledMethodTest >> testClone [
	<pragma: #pragma> "for testing"
	| method copy |
	method := thisContext method.
	self assert: method pragmas notEmpty.
	copy := method clone.
	self assert: (method equivalentTo: copy).
	self assert: method header equals: copy header.
	self assert: method equals: copy.
	self assert: method ~~ copy.
	"this is a shallow copy"
	self deny: copy penultimateLiteral method identicalTo: copy.
	copy pragmas do: [ :p | self deny: p method identicalTo: copy ]
]

{ #category : 'tests - accessing' }
CompiledMethodTest >> testComments [
	"I am the first comment to be found in this test"
	self
		assert: (CompiledMethodTest >> #testComments) comments first
	"And I am the second comment to be found in this test"
		equals: 'I am the first comment to be found in this test'.
	self
		assert: (CompiledMethodTest >> #testComments) comments second
		equals: 'And I am the second comment to be found in this test'.

	"Next test assumes #testComparison, in this same class has no comment..."
	self assert: (CompiledMethodTest >> #testComparison) comments isEmpty
]

{ #category : 'tests - accessing' }
CompiledMethodTest >> testComparison [
	| method1 method2 |
	method1 := Float class >> #nan.
	method2 := thisContext method.

	self assert: method1 equals: method1.
	self assert: method2 equals: method2.
	self deny: method1 equals: method2.
	self deny: method2 equals: method1.

	Object methods
		do: [ :each |
			self deny: method1 equals: each.
			self deny: each equals: method1.
			self deny: method2 equals: each.
			self deny: each equals: method2 ]
]

{ #category : 'tests - compilation' }
CompiledMethodTest >> testCompilingExistingMethodDoesNotRemoveExtensions [
	"Regression test for a case where recompiling a method removed the fact that it was an extension."

	[
	| method |
	self class compile: 'isExtensionTestMethod ^ 1' classified: '*GeneratedPackageForTest'.
	method := self class >> #isExtensionTestMethod.
	self assert: method isExtension.

	self class compile: 'isExtensionTestMethod ^ 2'.

	self assert: method isExtension ] ensure: [
		self packageOrganizer removePackage: 'GeneratedPackageForTest'.
		self class compiledMethodAt: #isExtensionTestMethod ifPresent: [ :method | method removeFromSystem ] ]
]

{ #category : 'tests - copying' }
CompiledMethodTest >> testCopy [
	<pragma: #pragma>
	| method copy |
	method := thisContext method.
	self assert: method pragmas notEmpty.
	copy := method copy.
	self assert: (method equivalentTo: copy).
	self assert: method header equals: copy header.
	self assert: method equals: copy.
	self assert: method ~~ copy.
	self assert: copy penultimateLiteral method identicalTo: copy.
	self assert: method penultimateLiteral method identicalTo: method.
	method pragmas do: [ :p | self assert: p method identicalTo: method ].
	copy pragmas do: [ :p | self assert: p method identicalTo: copy ]
]

{ #category : 'tests - comparing' }
CompiledMethodTest >> testEqualityClassSideMethod [
   	| method1 method2 |

	method1 := TestCase class compiler
			compile: 'aMethod'.

	method2 := TestCase class compiler
			compile: 'aMethod'.

 	self assert: method1 equals: method2
]

{ #category : 'tests - comparing' }
CompiledMethodTest >> testEqualityInstanceSideMethod [
	| method1 method2 |
	method1 := TestCase compiler compile: 'aMethod'.
	method2 := TestCase compiler compile: 'aMethod'.

	self assert: (method1 literalAt: method1 numLiterals) identicalTo: (method2 literalAt: method2 numLiterals).
	self assert: method1 equals: method2
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testHasComment [

	self assert: (CompiledMethodTest >> #testComments) hasComment
]

{ #category : 'tests - literals' }
CompiledMethodTest >> testHasLiteralSuchThat [
	"#literals should not expose implementation hack that the last two literals are
	used for name of method and class"

	self deny: (Object >> #yourself hasLiteralSuchThat: [ :lit | lit == #yourself ]).
	self assert: (Object >> #halt hasLiteralSuchThat: [ :lit | lit == #now ])
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testHasNonLocalReturn [
	| method |
	method := self class compiler compile: 'm ^1'.
	self deny: [method hasNonLocalReturn ].
	method := self class compiler compile: 'm self doSomething'.
	self deny: [method hasNonLocalReturn ].
	method := self class compiler compile: 'm []'.
	self deny: [method hasNonLocalReturn ].
	method := self class compiler compile: 'm [^self]'.
	self assert: [method hasNonLocalReturn ]
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testHasPragma [

	| methodWithPragma methodWithOutPragma |
	methodWithPragma := (SmallInteger >> #+).
	methodWithOutPragma := thisContext method.

	self assert: methodWithPragma hasPragma.
	self deny: methodWithOutPragma hasPragma
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testIsAbstract [

	self assert: (self someClass >> #abstractMethod) isAbstract.
	self deny: (self someClass >> #nonAbstractMethod) isAbstract.
	self deny: (self someClass >> #shouldNotImplementMethod) isAbstract
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testIsClassSide [
	self deny: (Object>>#yourself) isClassSide.
	self assert: (UndefinedObject class>>#new) isClassSide
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testIsDeprecated [
	| deprecatedSelectors |
	deprecatedSelectors := #(deprecatedMethod deprecatedMethod2 deprecatedMethod3 deprecatedMethod4).
	self class selectorsDo: [ :each |
		(deprecatedSelectors includes: each)
			ifTrue: [ self assert: (self class >> each) isDeprecated ]
			ifFalse: [ self deny: (self class >> each) isDeprecated ] ]
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testIsExtension [

	[
	| method |
	self class compile: 'isExtensionTestMethod ^ 1' classified: 'protocol'.
	self deny: (self class >> #isExtensionTestMethod) isExtension.

	self class compile: 'isExtensionTestMethod ^ 1' classified: '*Kernel-Tests-Generated-Package'.
	method := self class >> #isExtensionTestMethod.
	self assert: method isExtension.

	self class removeSelector: #isExtensionTestMethod.
	"Now we keep the infos about extension methods even after removing them"
	self assert: method isExtension ] ensure: [
		self packageOrganizer removePackage: 'Kernel-Tests-Generated-Package'.
		self class compiledMethodAt: #isExtensionTestMethod ifPresent: [ :method | method removeFromSystem ] ]
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testIsFaulty [
	|  cm |
	cm := OpalCompiler new
				source: 'method 3+';
				permitFaulty: true;
				compile.
	self assert: cm isFaulty.
	self deny: (OCASTTranslator>>#visitParseErrorNode:) isFaulty
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testIsInstalled [

	| method |
	method := self someClass >> #returnTrue.
	self assert: method isInstalled.

	class := self someClass classInstaller make: [ :aBuilder |
		         aBuilder
			         name: #TUTU;
			         package: self packageNameForTests ].

	class compile: 'foo ^ 10'.
	method := class >> #foo.

	"now make an orphaned method by just deleting the method."
	class removeSelector: #foo.

	self deny: method isInstalled
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testIsInstanceSide [
	self assert: (Object>>#yourself) isInstanceSide.
	self deny: (UndefinedObject class>>#new) isInstanceSide
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testIsQuick [
	| method  |

	method := self someClass compiledMethodAt: #returnTrue.
	self assert: (method isQuick).

	method := self someClass compiledMethodAt: #returnPlusOne:.
	self deny: (method isQuick)
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeAllInOne [
	| method |
	method := self someClass >> #locComplex.
	self assert: method linesOfCode equals: 14
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeEmptyLineInTheEnd [
	| method |
	method := self someClass >> #locEmptyLineInTheEnd.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeEmptyLineInTheMiddle [
	| method |
	method := self someClass >> #locEmptyLineInTheMiddle.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeEmptyLineWithTabInTheEnd [
	| method |
	method := self someClass >> #locEmptyLineWithTabInTheEnd.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeEmptyLineWithTabInTheMiddle [
	| method |
	method := self someClass >> #locEmptyLineWithTabInTheMiddle.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeEmptyMethod [
	| method |
	method := self someClass >> #locEmptyMethod.
	self assert: method linesOfCode equals: 1
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeEmptyMethodWithNewline [
	| method |
	method := self someClass >> #locEmptyMethodWithNewline.
	self assert: method linesOfCode equals: 1
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeEmptyMethodWithNewlineAndTab [
	| method |
	method := self someClass >> #locEmptyMethodWithNewlineAndTab.
	self assert: method linesOfCode equals: 1
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeMultilineComment [
	| method |
	method := self someClass >> #locMultilineComment.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeMultilineCommentWithoutWhitespace [
	| method |
	method := self someClass >> #locMultilineCommentWithoutWhitespace.
	self assert: method linesOfCode equals: 6
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeMultilineMethodComment [
	| method |
	method := self someClass >> #locMultilineMethodComment.
	self assert: method linesOfCode equals: 4
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeMultilineMethodCommentWithoutWhitespace [
	| method |
	method := self someClass >> #locMultilineMethodCommentWithoutWhitespace.
	self assert: method linesOfCode equals: 3
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeSimpleCase [
	| method |
	method := self someClass >> #locSimple.
	self assert: method linesOfCode equals: 6
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeSingleLineComment [
	| method |
	method := self someClass >> #locSingleLineComment.
	self assert: method linesOfCode equals: 3
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeSingleLineCommentWithoutWhitespace [
	| method |
	method := self someClass >> #locSingleLineCommentWithoutWhitespace.
	self assert: method linesOfCode equals: 3
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeSingleLineMethodComment [
	| method |
	method := self someClass >> #locSingleLineMethodComment .
	self assert: method linesOfCode equals: 3
]

{ #category : 'tests - linesofcode' }
CompiledMethodTest >> testLinesOfCodeSingleLineMethodCommentWithoutWhitespace [
	| method |
	method := self someClass >> #locSingleLineMethodCommentWithoutWhitespace.
	self assert: method linesOfCode equals: 3
]

{ #category : 'tests - literals' }
CompiledMethodTest >> testLiterals [
	"#literals should not expose implementation hack that the last two literals are
	used for name of method and class"

	self assertEmpty: (Object >> #yourself) literals.
	self assert: (Object >> #yourself) allLiterals size equals: 2.
	self deny: (Object >> #yourself hasLiteral: #yourself)
]

{ #category : 'tests - accessing' }
CompiledMethodTest >> testMethodClass [

	| method |
	method := self someClass >> #returnTrue.
	self assert: method selector equals: #returnTrue.

	"now make an orphaned method by just deleting the class.
	old: #unknown
	new semantics: return Absolete class"
	class := self someClass classInstaller make: [ :aBuilder |
		         aBuilder
			         name: #TUTU;
			         package: self packageNameForTests ].

	class compile: 'foo ^ 10'.
	method := class >> #foo.
	class removeFromSystem.
	self assert: method methodClass equals: class
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testMethodInDeprecatedClassIsDeprecated [

	class := self class classInstaller make: [ :aBuilder |
		         aBuilder
			         name: #Derg;
			         package: self packageNameForTests ].
	class compile: 'method ^ 1'.

	self deny: class isDeprecated.
	self deny: (class >> #method) isDeprecated.

	class class compile: 'isDeprecated ^ true'.
	self assert: class isDeprecated.
	self assert: (class >> #method) isDeprecated
]

{ #category : 'tests - comparing' }
CompiledMethodTest >> testMethodsThatHaveOnlyDifferentSelectorsShouldBeDifferent [
	| method1 method2 |
	method1 := TestCase compiler compile: 'aMethod'.
	method2 := TestCase compiler compile: 'aMethod2'.

	self assert: (method1 literalAt: method1 numLiterals) identicalTo: (method2 literalAt: method2 numLiterals).
	self deny: method1 equals: method2
]

{ #category : 'tests - accessing' }
CompiledMethodTest >> testOrigin [
	| regularMethod methodFromTrait |
	"Regular method"
	regularMethod := Behavior >> #name.

	"Method from a trait without alias "
	methodFromTrait := Behavior >> #adoptInstance:.

	self assert: regularMethod origin identicalTo: regularMethod originMethod methodClass.
	self assert: methodFromTrait origin identicalTo: methodFromTrait originMethod methodClass
]

{ #category : 'tests - accessing' }
CompiledMethodTest >> testOverriddenMethod [

	self assert: (self class >> #classToBeTested) overriddenMethod identicalTo: ClassTestCase >> #classToBeTested.
	self assert: (self class >> #testOverriddenMethod) overriddenMethod isNil
]

{ #category : 'tests - performing' }
CompiledMethodTest >> testPerformCanExecutelongMethodWithTemps [
	"the perform: primitive reuses the context of the method calling it. The primitive adds performed selector arguments to the context variables list. So this means that you can execute some methods but not performed them if the calling methods defined too many temps "

	| temp1 temp2 temp3 |
	temp1 := 33.
	temp2 := 666.
	temp3 := 42.
	self assert: (self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15: withArguments: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15)) equals: 1.
	self assert: (self class>>#testPerformCanExecutelongMethodWithTemps) frameSize equals: CompiledMethod smallFrameSize.
	self assert: (self class>>#a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15:) frameSize equals: CompiledMethod fullFrameSize
]

{ #category : 'tests - performing' }
CompiledMethodTest >> testPerformInSuperclassCanExecutelongMethodWithTemps [
	"the perform: primitive reuses the context of the method calling it. The primitive adds performed selector arguments to the context variables list. So this means that you can execute some methods but not performed them if the calling methods defined too many temps "

	| temp1 temp2 temp3 |
	temp1 := 33.
	temp2 := 666.
	temp3 := 42.
	self assert: (self perform: #a1:a2:a3:a4:a5:a6:a7:a8:a9:a10:a11:a12:a13:a14:a15: withArguments: #(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15) inSuperclass: self class) equals: 1
]

{ #category : 'tests - accessing - pragmas & properties' }
CompiledMethodTest >> testPragmaAt [

	| method |
	method := self someClass >> #methodWithPragma.
	self assert: (method pragmaAt: #bebou) selector equals: #bebou.
	self assert: (method pragmaAt: #hello) equals: nil.
	method propertyAt: #hello put: true.
	"Pragmas are not Properties!"
	self assert: (method pragmaAt: #hello) equals: nil
]

{ #category : 'tests - accessing - pragmas & properties' }
CompiledMethodTest >> testProperties [

	| method tmp |
	method := self someClass >> #returnTrue.
	
	"No property. Unlike classic collections, nil is returned on absence"
	self deny: method hasProperties.
	self deny: (method hasProperty: #doesNotExist).
	self assert: (method propertyAt: #doesNotExist) equals: nil.
	self assert: (method propertyAt: #doesNotExist ifAbsent: [ #tag ]) equals: #tag.
	self assert: (method removeProperty: #doesNotExist) equals: nil.
	self assert: (method removeProperty: #doesNotExist ifAbsent: [ #tag ]) equals: #tag.
	
	"Add a property"
	self assert: (method propertyAt: #doesNotExist put: #yesItDoes) equals: #yesItDoes.
	self assert: method hasProperties.
	self assert: (method hasProperty: #doesNotExist).
	self assert: (method propertyAt: #doesNotExist) equals: #yesItDoes.
	self assert: (method propertyAt: #doesNotExist ifAbsent: [ #tag ]) equals: #yesItDoes.

	"Remove it"
	self assert: (method removeProperty: #doesNotExist) equals: #yesItDoes.
	self deny: method hasProperties.
	self assert: (method removeProperty: #doesNotExist) equals: nil.

	"Add it back"
	self assert: (method propertyAt: #doesNotExist ifAbsentPut: [ #yesItDoesAgain ]) equals: #yesItDoesAgain.
	self assert: method hasProperties.
	self assert: (method propertyAt: #doesNotExist) equals: #yesItDoesAgain.

	"Add a second one"
	self assert: (method propertyAt: #doesExist put: #yesItDoes) equals: #yesItDoes.
	
	tmp := OrderedCollection new.
	method propertyKeysAndValuesDo: [ :key :value | tmp add: { key . value } ].
	self assertCollection: tmp hasSameElements: #( #( #doesNotExist #yesItDoesAgain ) #(#doesExist #yesItDoes) ).
	
	"Remove both"
	method removeProperty: #doesNotExist.
	self assert: method hasProperties.
	self assert: (method propertyAt: #doesNotExist) equals: nil.
	self assert: (method propertyAt: #doesExist) equals: #yesItDoes.
	method removeProperty: #doesExist.
	self deny: method hasProperties
]

{ #category : 'tests - accessing - pragmas & properties' }
CompiledMethodTest >> testPropertyAtIfPresentDoNotClashWithPragmas [
	"This is a regression test because #propertyAt:ifPresent: was returning the pragmas of the same name."

	| method |
	method := self someClass >> #methodWithPragma.

	[
	method propertyAt: #bebou ifPresent: [ :value | self fail: 'There should be no property on this method.' ].

	method propertyAt: #bebou put: true.

	self assert: (method propertyAt: #bebou ifPresent: [ :value | value ]) ] ensure: [ method removeProperty: #bebou ]
]

{ #category : 'tests - compiling' }
CompiledMethodTest >> testProtocolOfRemovedMethod [

	| method |
	class := Object newAnonymousSubclass.
	class compile: 'billy ^ 1' classified: 'cat'.

	method := class >> #billy.

	self assert: method protocol name equals: #cat.
	self assert: method protocolName equals: #cat.

	class removeSelector: #billy.

	self assert: method protocol name equals: #cat.
	self assert: method protocolName equals: #cat
]

{ #category : 'tests - instance variable' }
CompiledMethodTest >> testReadsField [
	| method |
	method := self someClass compiledMethodAt: #readX.
	self assert: (method readsField: 4).

	method := self someClass compiledMethodAt: #readXandY.
	self assert: (method readsField: 5).


	"read is not write"
	method := self someClass compiledMethodAt: #writeX.
	self deny: (method readsField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self deny: (method readsField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self deny: (method readsField: 5)
]

{ #category : 'tests - slots' }
CompiledMethodTest >> testReadsSlot [

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode)
		ifFalse: [ ^ self ].

	self assert: ((Point>>#x) readsSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) readsSlot: (Point slotNamed: #y)).
	self deny: ((Point>>#setX:setY:) readsSlot: (Point slotNamed: #y))
]

{ #category : 'tests - compilation' }
CompiledMethodTest >> testRecompilingDoesNotRemoveExtensions [
	"Regression test for a case where recompiling a method removed the fact that it was an extension."

	[
	| method |
	self class compile: 'isExtensionTestMethod ^ 1' classified: '*GeneratedPackageForTest'.
	method := self class >> #isExtensionTestMethod.
	self assert: method isExtension.

	method recompile.

	self assert: method isExtension ] ensure: [
		self packageOrganizer removePackage: 'GeneratedPackageForTest'.
		self class compiledMethodAt: #isExtensionTestMethod ifPresent: [ :method | method removeFromSystem ] ]
]

{ #category : 'tests - accessing - pragmas & properties' }
CompiledMethodTest >> testRemovePropertyIfAbsentRemovesAdditionalMethodStateIfEmpty [

	| method |
	
	method := self someClass >> #returnTrue.
	method propertyAt: #aTestProperty put: 42.
	
	self assert: (method hasProperty: #aTestProperty).
	self assert: method penultimateLiteral isMethodProperties.
	self assert: method penultimateLiteral size equals: 1.
	
	method removeProperty: #aTestProperty ifAbsent: [ self fail ].
	
	self deny: (method hasProperty: #aTestProperty).
	self deny: method penultimateLiteral isMethodProperties.

]

{ #category : 'tests - accessing - pragmas & properties' }
CompiledMethodTest >> testRemovePropertyRemovesAdditionalMethodStateIfEmpty [

	| method |
	
	method := self someClass >> #returnTrue.
	method propertyAt: #aTestProperty put: 42.
	
	self assert: (method hasProperty: #aTestProperty).
	self assert: method penultimateLiteral isMethodProperties.
	self assert: method penultimateLiteral size equals: 1.
	
	method removeProperty: #aTestProperty.
	
	self deny: (method hasProperty: #aTestProperty).
	self deny: method penultimateLiteral isMethodProperties.

]

{ #category : 'tests - accessing' }
CompiledMethodTest >> testSelector [

	| method |
	method := self someClass >> #returnTrue.
	self assert: method selector equals: #returnTrue.

	"now make an orphaned method. new semantics: return corrent name"
	class := self someClass classInstaller make: [ :aBuilder |
		         aBuilder
			         name: #TUTU;
			         package: self packageNameForTests ].

	class compile: 'foo ^ 10'.

	method := class >> #foo.

	Smalltalk removeClassNamed: #TUTU.

	self assert: method selector equals: #foo
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testSendsSelector [
	self assert: ((CompiledCode >> #sendsSelector:) sendsSelector: #includes:).
	self deny: ((CompiledCode >> #sendsSelector:) sendsSelector: #doBreakfastForMe)
]

{ #category : 'tests - literals' }
CompiledMethodTest >> testUndeclaredReparationWithClass [

	| method c c2 |

	Smalltalk globals removeKey: #TestUndeclaredVariable ifAbsent: [].
	Undeclared removeKey: #TestUndeclaredVariable ifAbsent: []. "Because obsolete may remain"

	method := self class compiler permitFaulty: true; compile: 'x ^TestUndeclaredVariable ifNil: [ TestUndeclaredVariable ]'.
	self assert: method usesUndeclareds.
	self should: [ nil executeMethod: method ] raise: UndeclaredVariableRead.

	"Adding a class DOES repair"
	c := Object subclass: #TestUndeclaredVariable.
	self deny: method usesUndeclareds.
	self assert: (nil executeMethod: method) equals: c.

	"Removal of class DOES undeclare users"
	c removeFromSystem.
	self assert: method usesUndeclareds.
	self should: [nil executeMethod: method] raise: UndeclaredVariableRead.

	"Thus, adding back repair DOES repair again"
	c2 := Object subclass: #TestUndeclaredVariable.
	self deny: c equals: c2.
	self deny: method usesUndeclareds.
	self assert: (nil executeMethod: method) equals: c2.

	"cleanup"
	Smalltalk globals removeKey: #TestUndeclaredVariable
]

{ #category : 'tests - literals' }
CompiledMethodTest >> testUndeclaredReparationWithGlobal [

	| method |

	Smalltalk globals removeKey: #TestUndeclaredVariable ifAbsent: [].

	method := self class compiler permitFaulty: true; compile: 'x ^TestUndeclaredVariable ifNil: [ TestUndeclaredVariable ]'.
	self assert: method usesUndeclareds.
	self should: [ nil executeMethod: method ] raise: UndeclaredVariableRead.

	"Adding a global DOES repair"
	Smalltalk globals at: #TestUndeclaredVariable put: 42.
	self deny: method usesUndeclareds.
	self assert: (nil executeMethod: method) equals: 42.

	"Removal of globals DOES NOT undeclare users"
	Smalltalk globals removeKey: #TestUndeclaredVariable.
	self deny: method usesUndeclareds.
	self assert: (nil executeMethod: method) equals: 42.

	"Thus, adding back DOES NOT repair"
	Smalltalk globals at: #TestUndeclaredVariable put: 421.
	self deny: method usesUndeclareds.
	self assert: (nil executeMethod: method) equals: 42.

	"cleanup"
	Smalltalk globals removeKey: #TestUndeclaredVariable
]

{ #category : 'tests - literals' }
CompiledMethodTest >> testUndeclaredReparationWithInstanceVariable [

	| method method2 method3 method4 receiver |
	Smalltalk globals at: #TestUndeclaredVariableClass ifPresent: [ :c | c removeFromSystem ].
	class := (Object << #TestUndeclaredVariableClass package: self packageNameForTests) install.
	self assert: (class instVarIndexFor: #TestUndeclaredVariable) equals: 0.
	receiver := class new.

	"Note: unlike related tests, we need here a reveiver (see above) and to install the method in the class"
	method := class compiler
		          permitFaulty: true;
		          install: 'x ^TestUndeclaredVariable ifNil: [ TestUndeclaredVariable ]'.
	self assert: method usesUndeclareds.
	self should: [ receiver executeMethod: method ] raise: UndeclaredVariableRead.

	"Update the class (same identity, not a new class)"
	self
		assert: ((Object << #TestUndeclaredVariableClass)
				 slots: { #TestUndeclaredVariable };
				 package: self packageNameForTests) install
		equals: class.
	self assert: (class instVarIndexFor: #TestUndeclaredVariable) equals: 1.
	receiver instVarAt: 1 put: 42.

	"Adding ivars DOES not repair the CompiledMethod"
	self assert: method usesUndeclareds.
	"But recompile and install a NEW method"
	self deny: method equals: (method2 := class >> #x). "It is a new method"
	self deny: method2 usesUndeclareds.
	self assert: (receiver executeMethod: method2) equals: 42.

	"Removal of ivar DOES recompile a new new one"
	class := (Object << #TestUndeclaredVariableClass package: self packageNameForTests) install.
	self deny: method2 equals: (method3 := class >> #x). "It is a new method"
	self assert: method3 usesUndeclareds.
	self should: [ receiver executeMethod: method3 ] raise: UndeclaredVariableRead.

	"Thus adding back DOES recompile again"
	class := ((Object << #TestUndeclaredVariableClass)
		          slots: { #TestUndeclaredVariable };
		          package: self packageNameForTests) install.
	receiver instVarAt: 1 put: 421.
	self deny: method3 equals: (method4 := class >> #x). "It is a new method"
	self deny: method4 usesUndeclareds.
	self assert: (receiver executeMethod: method4) equals: 421
]

{ #category : 'tests - literals' }
CompiledMethodTest >> testUndeclaredReparationWithShared [

	| method |
	class := (Object << #TestUndeclaredVariableClass package: self packageNameForTests) install.
	method := class compiler
		          permitFaulty: true;
		          compile: 'x ^TestUndeclaredVariable ifNil: [ TestUndeclaredVariable ]'.
	"Add and initialize a shared"
	class := ((Object << #TestUndeclaredVariableClass)
		          sharedVariables: { #TestUndeclaredVariable };
		          package: self packageNameForTests) install.

	nil executeMethod: method.
	nil executeMethod: method
]

{ #category : 'tests - literals' }
CompiledMethodTest >> testUndeclaredReparationWithSharedWasCrashingOnOldVM1001 [

	| method |
	class := (Object << #TestUndeclaredVariableClass package: self packageNameForTests) install.
	method := class compiler
		          permitFaulty: true;
		          compile: 'x ^TestUndeclaredVariable ifNil: [ TestUndeclaredVariable ]'.
	"Add and initialize a shared"
	class := ((Object << #TestUndeclaredVariableClass)
		          sharedVariables: { #TestUndeclaredVariable };
		          package: self packageNameForTests) install.

	nil executeMethod: method.
	nil executeMethod: method
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testUsesUndeclareds [
	| method |
	method := self someClass compiler compile: 'x ^x'.
	self deny: method usesUndeclareds.
	method := self someClass compiler permitFaulty: true; compile: 'z ^z'.
	self assert: method usesUndeclareds.
	method := self someClass compiler permitFaulty: true; compile: 'msg self in: [ :anObject | var1 := 1 ]'.
	self assert: method usesUndeclareds
]

{ #category : 'tests - evaluating' }
CompiledMethodTest >> testValueWithReceiver [

	| method value |

	method := self someClass compiledMethodAt: #returnTrue.

	value := method valueWithReceiver: nil .
	self assert: value equals: true.
]

{ #category : 'tests - evaluating' }
CompiledMethodTest >> testValueWithReceiverArguments [

	| method value |

	method := self someClass compiledMethodAt: #returnTrue.

	value := method valueWithReceiver: nil.
	self assert: value equals: true.

	method := self someClass compiledMethodAt: #returnPlusOne:.
	value := method valueWithReceiver: nil arguments: #(1).
	self assert: value equals: 2
]

{ #category : 'tests - instance variable' }
CompiledMethodTest >> testWritesField [
	| method |
	method := self someClass compiledMethodAt: #writeX.
	self assert: (method writesField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self assert: (method writesField: 4).

	method := self someClass compiledMethodAt: #writeXandY.
	self assert: (method writesField: 5).

	"write is not read"

	method := self someClass compiledMethodAt: #readX.
	self deny: (method writesField: 4).

	method := self someClass compiledMethodAt: #readXandY.
	self deny: (method writesField: 4).

	method := self someClass compiledMethodAt: #readXandY.
	self deny: (method writesField: 5)
]

{ #category : 'tests - slots' }
CompiledMethodTest >> testWritesSlot [

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode)
		ifFalse: [ ^ self ].

	self deny: ((Point>>#x) writesSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) writesSlot: (Point slotNamed: #y)).

	self assert: ((Point>>#setX:setY:) writesSlot: (Point slotNamed: #y)).
	self assert: ((Point>>#setX:setY:) writesSlot: (Point slotNamed: #x))
]

{ #category : 'tests - testing' }
CompiledMethodTest >> testWritesUndeclared [
	| method |
	
	"x is an ivar"
	method := self someClass compiler compile: 'x ^ x := 0'.
	self deny: method usesUndeclareds.
	self assert: (self executeMethod: method) equals: 0.

	"undeclaredxyz is not declared"
	method := self someClass compiler permitFaulty: true; compile: 'z ^ undeclaredxyz := 1'.
	self assert: method usesUndeclareds.
	self should: [ self executeMethod: method ] raise: UndeclaredVariableWrite.
	self
		assert:
			([ self executeMethod: method ]
				on: UndeclaredVariableWrite
				do: [:e | e resume])
		equals: 1.

	"check same behavior in blocks"
	method := self someClass compiler permitFaulty: true; compile: 'msg ^ self in: [ :anObject | undeclaredxyz := 2 ]'.
	self assert: method usesUndeclareds.
	self should: [ self executeMethod: method ] raise: UndeclaredVariableWrite.
	self
		assert:
			([ self executeMethod: method ]
				on: UndeclaredVariableWrite
				do: [:e | e resume])
		equals: 2
]
