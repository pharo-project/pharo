Extension { #name : #ClassTrait }

{ #category : #'*OpalCompiler-Core' }
ClassTrait >> compiler [
	"The compiler is defined on instance-side to be able to customize it for *one* metaclass"
	^ self instanceSide classSideCompiler
]

{ #category : #'*OpalCompiler-Core' }
ClassTrait >> compilerClass [
	"The compiler class is defined on instance-side to be able to customize it for *one* metaclass"
	^ self instanceSide classSideCompilerClass
]
