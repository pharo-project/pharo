"
My strategy is to create an UndefinedClassin the case the class doesn't exist.
"
Class {
	#name : #ClassResolverUndefinedClass,
	#superclass : #ClassResolver,
	#category : #'UndefinedClasses-Core'
}

{ #category : #resolving }
ClassResolverUndefinedClass >> resolve: aClassName [
"should build an undefined class instead of creating (build + install)"
	^UndefinedClass createUndefinedClassNamed: aClassName package:  RPackage defaultPackageName.
]
