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
