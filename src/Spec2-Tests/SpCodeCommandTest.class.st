Class {
	#name : #SpCodeCommandTest,
	#superclass : #TestCase,
	#category : #'Spec2-Tests-Commands'
}

{ #category : #testing }
SpCodeCommandTest class >> isAbstract [

	^ self = SpCodeCommandTest
]

{ #category : #testing }
SpCodeCommandTest class >> shouldInheritSelectors [

	^ true
]

{ #category : #accessing }
SpCodeCommandTest >> commandClass [
		
	^ self subclassResponsibility
]

{ #category : #accessing }
SpCodeCommandTest >> commandToTest [

	^ self commandClass new 
		context: self newMockContext;
		yourself
]

{ #category : #private }
SpCodeCommandTest >> newMockContext [
	
	^ SpCodeCommandContextMock new
]

{ #category : #tests }
SpCodeCommandTest >> testExecute [ 

	self flag: #TODO. "How to intercept messages to prevent execution?"
]
