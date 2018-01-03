Extension { #name : #CompiledMethodTest }

{ #category : #'*Kernel-Tests-WithCompiler' }
CompiledMethodTest >> testCompiledMethodAsString [

	thisContext method asString
]

{ #category : #'*Kernel-Tests-WithCompiler' }
CompiledMethodTest >> testContainsHalt [
	
	| annonClass |
	
	self 
	deny: (Object >> #halt) containsHalt;
	deny: (Object >> #haltIfNil) containsHalt;
	deny: (Halt class >> #once) containsHalt;
	assert: (UndefinedObjectTest >> #testIfNil) containsHalt.
	
	annonClass := Object newAnonymousSubclass.
	annonClass 
	compile: 'm1 self halt';
	compile: 'm2 self haltIfNil';
	compile: 'm3 self yourself'.
	
	self
	assert: (annonClass >> #m1) containsHalt;
	assert: (annonClass >> #m2) containsHalt;
	deny: (annonClass >> #m3) containsHalt.
	
	
]

{ #category : #'*Kernel-Tests-WithCompiler' }
CompiledMethodTest >> testEqualityClassSideMethod [
   	| method1 method2 |

	method1 := TestCase class compiler
			compile: 'aMethod'.
		
	method2 := TestCase class compiler
			compile: 'aMethod'.
			

 	self assert: method1 = method2.


]

{ #category : #'*Kernel-Tests-WithCompiler' }
CompiledMethodTest >> testEqualityInstanceSideMethod [
   	| method1 method2 |

	method1 :=  TestCase compiler compile: 'aMethod'.
	method2 :=  TestCase compiler compile: 'aMethod'.

	self assert: (method1 literalAt: method1 numLiterals) ==  (method2 literalAt: method2 numLiterals).
 	self assert: method1 = method2.


]

{ #category : #'*Kernel-Tests-WithCompiler' }
CompiledMethodTest >> testIsInstalled [
|  method cls |

	method := (self class)>>#returnTrue.
	self assert: method isInstalled.

	"now make an orphaned method by just deleting the class."

	Smalltalk removeClassNamed: #TUTU.

	cls := Object subclass: #TUTU
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: self categoryNameForTemporaryClasses.
	cls compile: 'foo ^ 10'.
	method := cls >> #foo.
	Smalltalk removeClassNamed: #TUTU.

	self deny: method isInstalled. 
]

{ #category : #'*Kernel-Tests-WithCompiler' }
CompiledMethodTest >> testMethodClass [
	| method cls |
	method := self class >> #returnTrue.
	self assert: method selector = #returnTrue.
	"now make an orphaned method by just deleting the class.
	old: #unknown
	new semantics: return Absolete class"
	Smalltalk removeClassNamed: #TUTU.
	cls := Object
				subclass: #TUTU
				instanceVariableNames: ''
				classVariableNames: ''
				poolDictionaries: ''
				category: self categoryNameForTemporaryClasses.
	cls compile: 'foo ^ 10'.
	method := cls >> #foo.
	Smalltalk removeClassNamed: #TUTU.
	self assert: method methodClass = cls
]

{ #category : #'*Kernel-Tests-WithCompiler' }
CompiledMethodTest >> testSelector [

	Author useAuthor: 'TUTU_TEST' during: [ 
		
		|  method cls |
		
		method := (self class)>>#returnTrue.
		self assert: (method selector = #returnTrue).
		
		"now make an orphaned method. new semantics: return corrent name"	
		Smalltalk removeClassNamed: #TUTU.

		cls := Object subclass: #TUTU
			instanceVariableNames: ''
			classVariableNames: ''
			poolDictionaries: ''
			category: self categoryNameForTemporaryClasses.
		
		cls compile: 'foo ^ 10'.
		
		method := cls >> #foo.
		
		Smalltalk removeClassNamed: #TUTU.

		self assert: method selector = #foo. 
		
	].
	


]
