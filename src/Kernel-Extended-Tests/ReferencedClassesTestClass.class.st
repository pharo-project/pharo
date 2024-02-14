Class {
	#name : 'ReferencedClassesTestClass',
	#superclass : 'Object',
	#category : 'Kernel-Extended-Tests-Classes',
	#package : 'Kernel-Extended-Tests',
	#tag : 'Classes'
}

{ #category : 'methods' }
ReferencedClassesTestClass >> methodReferencingDirectly [
	^ Object
]

{ #category : 'methods' }
ReferencedClassesTestClass >> methodReferencingInBlock [
	^ [ OrderedCollection ]
]

{ #category : 'methods' }
ReferencedClassesTestClass >> methodReferencingInNestedBlock [
	^ [[[ Array ]]]
]
