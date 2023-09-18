Class {
	#name : 'ReferencedClassesTestClass',
	#superclass : 'Object',
	#category : 'Kernel-Tests-Extended-Classes',
	#package : 'Kernel-Tests-Extended',
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
