Class {
	#name : 'CompiledMethodCompilerTest',
	#superclass : 'ClassTestCase',
	#instVars : [
		'someClass',
		'class'
	],
	#category : 'Kernel-Extended-Tests-Methods',
	#package : 'Kernel-Extended-Tests',
	#tag : 'Methods'
}

{ #category : 'coverage' }
CompiledMethodCompilerTest >> classToBeTested [

	^ CompiledMethod
]

{ #category : 'running' }
CompiledMethodCompilerTest >> packageNameForTests [
	^ #'Generated-Compiled-Method-Test-Package'
]

{ #category : 'running' }
CompiledMethodCompilerTest >> setUp [
	super setUp.

	someClass := SomeClassForCompiledMethodTests
]

{ #category : 'running' }
CompiledMethodCompilerTest >> someClass [

	^ someClass 
]

{ #category : 'running' }
CompiledMethodCompilerTest >> tearDown [

	self packageOrganizer removePackage: self packageNameForTests.
	class ifNotNil: [ class removeFromSystem ].
	
	someClass compile: 'returnTrue
	^true'.
	
	super tearDown 
]

{ #category : 'tests - slots' }
CompiledMethodCompilerTest >> testAccessesSlot [

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode)
		ifFalse: [ ^ self ].

	self assert: ((Point>>#x) accessesSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) accessesSlot: (Point slotNamed: #y)).
	self assert: ((Point>>#setX:setY:) accessesSlot: (Point slotNamed: #y))
]

{ #category : 'tests - compilation' }
CompiledMethodCompilerTest >> testAddingSlotDoesNotRemoveExtension [
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
CompiledMethodCompilerTest >> testAsString [

	self assert: thisContext method asString isString
]

{ #category : 'tests - accessing' }
CompiledMethodCompilerTest >> testBytecode [
	"The result of this test depends on the used bytecode set.

	 Because there are multiple versions of the encoders currently depending on the compiler used, we test the class name instead of the encoder class itself"
	| method expectedResult |
	method := Object>>#halt.
	expectedResult := (method encoderClass name endsWith: 'SistaV1')
		ifTrue: [#[16 129 216 88]]
		ifFalse: [#[64 209 135 120]].
	self assertCollection: (Object>>#halt) bytecodes equals: expectedResult
]

{ #category : 'tests - accessing' }
CompiledMethodCompilerTest >> testComments [
	"I am the first comment to be found in this test"
	self
		assert: (CompiledMethodCompilerTest >> #testComments) comments first
	"And I am the second comment to be found in this test"
		equals: 'I am the first comment to be found in this test'.
	self
		assert: (CompiledMethodCompilerTest >> #testComments) comments second
		equals: 'And I am the second comment to be found in this test'.

	"Next test assumes #testComparison, in this same class has no comment..."
	self assert: (CompiledMethodCompilerTest >> #testComparison) comments isEmpty
]

{ #category : 'tests - accessing' }
CompiledMethodCompilerTest >> testComparison [
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
CompiledMethodCompilerTest >> testCompilingExistingMethodDoesNotRemoveExtensions [
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
CompiledMethodCompilerTest >> testCopy [
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
CompiledMethodCompilerTest >> testEqualityClassSideMethod [
   	| method1 method2 |

	method1 := TestCase class compiler
			source: 'aMethod';
			compileOrFail.

	method2 := TestCase class compiler
			source: 'aMethod';
			compileOrFail.

 	self assert: method1 equals: method2
]

{ #category : 'tests - comparing' }
CompiledMethodCompilerTest >> testEqualityInstanceSideMethod [
	| method1 method2 |
	method1 := TestCase compiler
			source: 'aMethod';
			compileOrFail.
	method2 := TestCase compiler
			source: 'aMethod';
			compileOrFail.

	self assert: (method1 literalAt: method1 numLiterals) identicalTo: (method2 literalAt: method2 numLiterals).
	self assert: method1 equals: method2
]

{ #category : 'tests - testing' }
CompiledMethodCompilerTest >> testHasComment [

	self assert: (CompiledMethodCompilerTest >> #testComments) hasComment
]

{ #category : 'tests - literals' }
CompiledMethodCompilerTest >> testHasLiteralSuchThat [
	"#literals should not expose implementation hack that the last two literals are
	used for name of method and class"

	self deny: (Object >> #yourself hasLiteralSuchThat: [ :lit | lit == #yourself ]).
	self assert: (Object >> #halt hasLiteralSuchThat: [ :lit | lit == #now ])
]

{ #category : 'tests - testing' }
CompiledMethodCompilerTest >> testHasNonLocalReturn [
	| method |
	method := self class compiler source: 'm ^1'; compileOrFail.
	self deny: [method hasNonLocalReturn ].
	method := self class compiler source: 'm self doSomething'; compileOrFail.
	self deny: [method hasNonLocalReturn ].
	method := self class compiler source: 'm []'; compileOrFail.
	self deny: [method hasNonLocalReturn ].
	method := self class compiler source: 'm [^self]'; compileOrFail.
	self assert: [method hasNonLocalReturn ]
]

{ #category : 'tests - testing' }
CompiledMethodCompilerTest >> testHasPragma [

	| methodWithPragma methodWithOutPragma |
	methodWithPragma := (SmallInteger >> #+).
	methodWithOutPragma := thisContext method.

	self assert: methodWithPragma hasPragma.
	self deny: methodWithOutPragma hasPragma
]

{ #category : 'tests - testing' }
CompiledMethodCompilerTest >> testIsClassSide [
	self deny: (Object>>#yourself) isClassSide.
	self assert: (UndefinedObject class>>#new) isClassSide
]

{ #category : 'tests - testing' }
CompiledMethodCompilerTest >> testIsDeprecated [
	| deprecatedSelectors |
	deprecatedSelectors := #(deprecatedMethod deprecatedMethod2 deprecatedMethod3 deprecatedMethod4).
	self class selectorsDo: [ :each |
		(deprecatedSelectors includes: each)
			ifTrue: [ self assert: (self class >> each) isDeprecated ]
			ifFalse: [ self deny: (self class >> each) isDeprecated ] ]
]

{ #category : 'tests - testing' }
CompiledMethodCompilerTest >> testIsExtension [

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
CompiledMethodCompilerTest >> testIsFaulty [
	|  cm |
	cm := OpalCompiler new
				source: 'method 3+';
				permitFaulty: true;
				compileOrFail.
	self assert: cm isFaulty.
	self deny: (OCASTTranslator>>#visitParseErrorNode:) isFaulty
]

{ #category : 'tests - testing' }
CompiledMethodCompilerTest >> testIsInstalled [

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
CompiledMethodCompilerTest >> testIsInstanceSide [
	self assert: (Object>>#yourself) isInstanceSide.
	self deny: (UndefinedObject class>>#new) isInstanceSide
]

{ #category : 'tests - literals' }
CompiledMethodCompilerTest >> testLiterals [
	"#literals should not expose implementation hack that the last two literals are
	used for name of method and class"

	self assertEmpty: (Object >> #yourself) literals.
	self assert: (Object >> #yourself) allLiterals size equals: 2.
	self deny: (Object >> #yourself hasLiteral: #yourself)
]

{ #category : 'tests - accessing' }
CompiledMethodCompilerTest >> testMethodClass [

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
CompiledMethodCompilerTest >> testMethodInDeprecatedClassIsDeprecated [

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
CompiledMethodCompilerTest >> testMethodsThatHaveOnlyDifferentSelectorsShouldBeDifferent [
	| method1 method2 |
	method1 := TestCase compiler source: 'aMethod'; compileOrFail.
	method2 := TestCase compiler source: 'aMethod2'; compileOrFail.

	self assert: (method1 literalAt: method1 numLiterals) identicalTo: (method2 literalAt: method2 numLiterals).
	self deny: method1 equals: method2
]

{ #category : 'tests - accessing' }
CompiledMethodCompilerTest >> testOrigin [
	| regularMethod methodFromTrait |
	"Regular method"
	regularMethod := Behavior >> #name.

	"Method from a trait without alias "
	methodFromTrait := Behavior >> #adoptInstance:.

	self assert: regularMethod origin identicalTo: regularMethod originMethod methodClass.
	self assert: methodFromTrait origin identicalTo: methodFromTrait originMethod methodClass
]

{ #category : 'tests - accessing' }
CompiledMethodCompilerTest >> testOverriddenMethod [
	
	self assert: (self class >> #classToBeTested) overriddenMethod identicalTo: ClassTestCase >> #classToBeTested.
	self assert: (self class >> #testOverriddenMethod) overriddenMethod isNil
]

{ #category : 'compiling' }
CompiledMethodCompilerTest >> testProtocolOfRemovedMethod [

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

{ #category : 'tests - slots' }
CompiledMethodCompilerTest >> testReadsSlot [

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode)
		ifFalse: [ ^ self ].

	self assert: ((Point>>#x) readsSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) readsSlot: (Point slotNamed: #y)).
	self deny: ((Point>>#setX:setY:) readsSlot: (Point slotNamed: #y))
]

{ #category : 'tests - compilation' }
CompiledMethodCompilerTest >> testRecompilingDoesNotRemoveExtensions [
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

{ #category : 'tests - accessing' }
CompiledMethodCompilerTest >> testSelector [

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
CompiledMethodCompilerTest >> testSendsSelector [
	self assert: ((CompiledCode >> #sendsSelector:) sendsSelector: #includes:).
	self deny: ((CompiledCode >> #sendsSelector:) sendsSelector: #doBreakfastForMe)
]

{ #category : 'tests - copying' }
CompiledMethodCompilerTest >> testShallowCopy [
	<pragma: #pragma> "for testing"
	| method copy |
	method := thisContext method.
	self assert: method pragmas notEmpty.
	copy := method shallowCopy.
	self assert: (method equivalentTo: copy).
	self assert: method header equals: copy header.
	self assert: method equals: copy.
	self assert: method ~~ copy.
	"this is a shallow copy"
	self deny: copy penultimateLiteral method identicalTo: copy.
	copy pragmas do: [ :p | self deny: p method identicalTo: copy ]
]

{ #category : 'tests - literals' }
CompiledMethodCompilerTest >> testUndeclaredReparationWithClass [

	| method c c2 |

	Smalltalk globals removeKey: #TestUndeclaredVariable ifAbsent: [].
	Undeclared removeKey: #TestUndeclaredVariable ifAbsent: []. "Because obsolete may remain"

	method := self class compiler permitFaulty: true; source: 'x ^TestUndeclaredVariable ifNil: [ TestUndeclaredVariable ]'; compileOrFail.
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
CompiledMethodCompilerTest >> testUndeclaredReparationWithGlobal [

	| method |

	Smalltalk globals removeKey: #TestUndeclaredVariable ifAbsent: [].

	method := self class compiler permitFaulty: true; source: 'x ^TestUndeclaredVariable ifNil: [ TestUndeclaredVariable ]'; compileOrFail.
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
CompiledMethodCompilerTest >> testUndeclaredReparationWithInstanceVariable [

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
CompiledMethodCompilerTest >> testUndeclaredReparationWithShared [

	| method |
	class := (Object << #TestUndeclaredVariableClass package: self packageNameForTests) install.
	method := class compiler
		          permitFaulty: true;
		          source: 'x ^TestUndeclaredVariable ifNil: [ TestUndeclaredVariable ]';
		          compileOrFail.
	
	"Add and initialize a shared"
	class := ((Object << #TestUndeclaredVariableClass)
		          sharedVariables: { #TestUndeclaredVariable };
		          package: self packageNameForTests) install.

	nil executeMethod: method.
	nil executeMethod: method
]

{ #category : 'tests - literals' }
CompiledMethodCompilerTest >> testUndeclaredReparationWithSharedWasCrashingOnOldVM1001 [

	| method |
	class := (Object << #TestUndeclaredVariableClass package: self packageNameForTests) install.
	method := class compiler
		          permitFaulty: true;
		          source: 'x ^TestUndeclaredVariable ifNil: [ TestUndeclaredVariable ]';
		          compileOrFail.
	
	"Add and initialize a shared"
	class := ((Object << #TestUndeclaredVariableClass)
		          sharedVariables: { #TestUndeclaredVariable };
		          package: self packageNameForTests) install.

	nil executeMethod: method.
	nil executeMethod: method
]

{ #category : 'tests - slots' }
CompiledMethodCompilerTest >> testWritesSlot [

	"Check the source code availability to do not fail on images without sources"
	({ Point>>#x. Point>>#setX:setY: } allSatisfy: #hasSourceCode)
		ifFalse: [ ^ self ].

	self deny: ((Point>>#x) writesSlot: (Point slotNamed: #x)).
	self deny: ((Point>>#x) writesSlot: (Point slotNamed: #y)).

	self assert: ((Point>>#setX:setY:) writesSlot: (Point slotNamed: #y)).
	self assert: ((Point>>#setX:setY:) writesSlot: (Point slotNamed: #x))
]
