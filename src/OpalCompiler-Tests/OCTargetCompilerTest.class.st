Class {
	#name : 'OCTargetCompilerTest',
	#superclass : 'TestCase',
	#category : 'OpalCompiler-Tests-Semantic',
	#package : 'OpalCompiler-Tests',
	#tag : 'Semantic'
}

{ #category : 'tests' }
OCTargetCompilerTest >> testModifiedReturn [
	" I test that the compilerClass method works for instance-side "
	
 	"It is possible that the method was loaded before the plugin was active. We recompile to be sure the plugin was active."
	OCTargetCompilerSample recompile.
	self assert: OCTargetCompilerSample new returnExpected equals: #expectedReturn
]

{ #category : 'tests' }
OCTargetCompilerTest >> testModifiedReturnFromClass [
	" I test that the compilerClass method works for class-side "

	self assert: OCTargetCompilerSample returnExpected equals: #expectedReturn
]

{ #category : 'tests' }
OCTargetCompilerTest >> testRecompiledReturn [
	" I test that the compilerClass method works for instance-side when we trigger method recompilation by adding/removing a slot for the class"
	
	self
		assert: ([
				OCTargetCompilerSample addSlot: #xyzzy. " forces recompile of methods "
				OCTargetCompilerSample new returnExpected
			] ensure: [
				OCTargetCompilerSample removeSlot: (OCTargetCompilerSample slotNamed: #xyzzy)
			])
 		equals: #expectedReturn
]

{ #category : 'tests' }
OCTargetCompilerTest >> testRecompiledReturnFromClass [
	" I test that the compilerClass method works for class-side when we trigger method recompilation by adding/removing a slot for the metaclass "
	self
		assert: ([
				OCTargetCompilerSample class addSlot: #xyzzy. " forces recompile of methods "
				OCTargetCompilerSample returnExpected
			] ensure: [
				OCTargetCompilerSample class removeSlot: (OCTargetCompilerSample class slotNamed: #xyzzy)
			])
 		equals: #expectedReturn
]
