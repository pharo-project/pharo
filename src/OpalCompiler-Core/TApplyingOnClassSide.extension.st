Extension { #name : #TApplyingOnClassSide }

{ #category : #'*OpalCompiler-Core' }
TApplyingOnClassSide >> compiler [
	"The compiler is defined on instance-side to be able to customize it for *one* metaclass"
	^ self instanceSide classSideCompiler
]

{ #category : #'*OpalCompiler-Core' }
TApplyingOnClassSide >> compilerClass [
	"The compiler class is defined on instance-side to be able to customize it for *one* metaclass"
	^ self instanceSide classSideCompilerClass
]
