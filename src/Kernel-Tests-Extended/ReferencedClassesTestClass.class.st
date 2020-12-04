Class {
	#name : #ReferencedClassesTestClass,
	#superclass : #Object,
	#category : #'Kernel-Tests-Extended-Classes'
}

{ #category : #'as yet unclassified' }
ReferencedClassesTestClass >> methodReferencingDirectly [ 
	^ Object
]

{ #category : #'as yet unclassified' }
ReferencedClassesTestClass >> methodReferencingInBlock [
	^ [ OrderedCollection ]
]

{ #category : #'as yet unclassified' }
ReferencedClassesTestClass >> methodReferencingInNestedBlock [
	^ [[[ Array ]]]
]
