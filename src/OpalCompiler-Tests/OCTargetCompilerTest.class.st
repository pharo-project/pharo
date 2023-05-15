Class {
	#name : #OCTargetCompilerTest,
	#superclass : #TestCase,
	#category : #'OpalCompiler-Tests-Semantic'
}

{ #category : #testing }
OCTargetCompilerTest >> expectedFailures [
	^ #(testModifiedReturnFromClass testRecompiledReturnFromClass)
]

{ #category : #tests }
OCTargetCompilerTest >> testModifiedReturn [
	" I test that the compilerClass method works for instance-side "
	" if something has failed to recompile using compilerClass, then you may need to go to OCTargetCompilerSample and force recompile of the method returnExpected (by e.g. adding/deleting a space and saving) "
	self skipOnPharoCITestingEnvironment.
	self assert: OCTargetCompilerSample new returnExpected equals: #expectedReturn
]

{ #category : #tests }
OCTargetCompilerTest >> testModifiedReturnFromClass [
	" I test that the compilerClass method works for class-side "
	" it ***doesn't*** at the moment!!!! "
	" if something has failed to recompile using compilerClass, then you may need to go to OCTargetCompilerSample class and force recompile of the method returnExpected (by e.g. adding/deleting a space and saving) "
	self assert: OCTargetCompilerSample returnExpected equals: #expectedReturn
]

{ #category : #tests }
OCTargetCompilerTest >> testRecompiledReturn [
	" I test that the compilerClass method works for instance-side when we trigger method recompilation by adding/removing a slot for the class "
	" it ***doesn't*** at the moment!!!! "
	self
		assert: ([
				OCTargetCompilerSample addSlot: #xyzzy. " forces recompile of methods "
				OCTargetCompilerSample new returnExpected
			] ensure: [
				OCTargetCompilerSample removeSlot: (OCTargetCompilerSample slotNamed: #xyzzy)
			])
 		equals: #expectedReturn
]

{ #category : #tests }
OCTargetCompilerTest >> testRecompiledReturnFromClass [
	" I test that the compilerClass method works for class-side when we trigger method recompilation by adding/removing a slot for the metaclass "
	" it ***doesn't*** at the moment!!!! "
	self
		assert: ([
				OCTargetCompilerSample class addSlot: #xyzzy. " forces recompile of methods "
				OCTargetCompilerSample returnExpected
			] ensure: [
				OCTargetCompilerSample class removeSlot: (OCTargetCompilerSample class slotNamed: #xyzzy)
			])
 		equals: #expectedReturn
]
