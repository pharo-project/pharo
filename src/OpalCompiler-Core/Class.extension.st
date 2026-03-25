Extension { #name : 'Class' }

{ #category : '*OpalCompiler-Core' }
Class >> classSideCompiler [
	"Redefine this method if you want to customize the compiler for class-side methods"
	^self classSideCompilerClass new
		environment: self classSide environment;
		class: self classSide
]

{ #category : '*OpalCompiler-Core' }
Class >> classSideCompilerClass [
	"Redefine this method if you want to customize the compiler class for class-side methods"
	^ Smalltalk compilerClass
]

{ #category : '*OpalCompiler-Core' }
Class >> compileAllFrom: oldClass [
	"Recompile all the methods in the receiver's method dictionary (not the
	subclasses). Also recompile the methods in the metaclass."

	super compileAllFrom: oldClass.
	self classSide compileAllFrom: oldClass classSide
]
