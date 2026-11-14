"
I refer to ReClassReferencedByAnother, to test that renaming it updates my method.
"
Class {
	#name : 'ReClassReferencingAnother',
	#superclass : 'Object',
	#category : 'Refactoring-DataForTesting-ForRenaming',
	#package : 'Refactoring-DataForTesting',
	#tag : 'ForRenaming'
}

{ #category : 'instance creation' }
ReClassReferencingAnother >> use [

	^ ReClassReferencedByAnother new
]
