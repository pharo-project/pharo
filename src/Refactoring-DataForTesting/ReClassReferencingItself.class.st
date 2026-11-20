"
I am used to test renaming a class whose own methods, on both sides, refer to it.
"
Class {
	#name : 'ReClassReferencingItself',
	#superclass : 'Object',
	#category : 'Refactoring-DataForTesting-ForRenaming',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForRenaming'
}

{ #category : 'instance creation' }
ReClassReferencingItself class >> build [

	^ ReClassReferencingItself new
]

{ #category : 'instance creation' }
ReClassReferencingItself >> make [

	^ ReClassReferencingItself new
]
