Extension { #name : 'Metaclass' }

{ #category : '*OpalCompiler-Core' }
Metaclass >> compiler [
	"The compiler is defined on instance-side to be able to customize it for *one* metaclass"
	^ self instanceSide classSideCompiler
]

{ #category : '*OpalCompiler-Core' }
Metaclass >> compilerClass [
	"The compiler class is defined on instance-side to be able to customize it for *one* metaclass"
	^ self instanceSide classSideCompilerClass
]
