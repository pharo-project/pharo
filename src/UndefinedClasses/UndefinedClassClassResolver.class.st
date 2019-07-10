"
I am an extension to the shift class builder in charge of resolving names.
My strategy is to create an UndefinedClass in the case the resolved name doesn't exist.
"
Class {
	#name : #UndefinedClassClassResolver,
	#superclass : #ClassResolver,
	#category : #'UndefinedClasses-Core'
}

{ #category : #resolving }
UndefinedClassClassResolver >> resolve: aClassName [
	"should build an undefined class instead of creating (build + install)"

	^ UndefinedClass
		createUndefinedClassNamed: aClassName
		package: RPackage defaultPackageName
]
