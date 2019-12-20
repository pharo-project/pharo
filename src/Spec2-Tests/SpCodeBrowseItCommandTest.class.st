Class {
	#name : #SpCodeBrowseItCommandTest,
	#superclass : #SpCodeCommandTest,
	#category : #'Spec2-Tests-Commands'
}

{ #category : #accessing }
SpCodeBrowseItCommandTest >> commandClass [
		
	^ SpCodeBrowseItCommand
]

{ #category : #tests }
SpCodeBrowseItCommandTest >> testFindClassOn [
	| command |
	
	command := self commandToTest.
	
	self assert: (command findClassOn: '') equals: nil.
	self assert: (command findClassOn: 'Object') equals: Object.
	self assert: (command findClassOn: 'Object.') equals: Object.	
	self assert: (command findClassOn: '.Object.') equals: Object.
	self assert: (command findClassOn: 'somethingBefore := 42.Object') equals: Object.	
	self 
		assert: (command findClassOn: 'somethingBefore := 42.Object. somethingAfter := 11') 
		equals: Object.	
	self assert: (command findClassOn: 'NonExistingClass.Object.') equals: Object.
]
