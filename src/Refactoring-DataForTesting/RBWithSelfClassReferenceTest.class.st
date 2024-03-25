<<<<<<< HEAD
Class {
	#name : 'RBWithSelfClassReferenceTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-DataForTesting-ForTestRelatedOperation',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForTestRelatedOperation'
}

{ #category : 'accessing' }
RBWithSelfClassReferenceTest >> method [

	^ RBWithSelfClassReferenceTest
]
||||||| ec979f6c13
=======
Class {
	#name : 'RBWithSelfClassReferenceTest',
	#superclass : 'TestCase',
	#category : 'Refactoring-DataForTesting-ForTestRelatedOperation',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForTestRelatedOperation'
}

{ #category : 'testing' }
RBWithSelfClassReferenceTest class >> isAbstract [

	^ self == RBWithSelfClassReferenceTest
]

{ #category : 'accessing' }
RBWithSelfClassReferenceTest >> method [

	^ RBWithSelfClassReferenceTest
]
>>>>>>> add_subclass_refactoring_driver
