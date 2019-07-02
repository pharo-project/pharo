Class {
	#name : #SycMockRBRefactoring,
	#superclass : #Object,
	#category : #'SystemCommands-RefactoringSupport-Test'
}

{ #category : #accessing }
SycMockRBRefactoring >> environment: anEnvironement [
	^ self
]

{ #category : #accessing }
SycMockRBRefactoring >> model [
	^ self
]

{ #category : #accessing }
SycMockRBRefactoring >> primitiveExecute [ 
	^self 
]

{ #category : #accessing }
SycMockRBRefactoring >> whatToDisplayIn: aBrowser [ 
	^ aBrowser scopeDropList selectedItem content
	
]
