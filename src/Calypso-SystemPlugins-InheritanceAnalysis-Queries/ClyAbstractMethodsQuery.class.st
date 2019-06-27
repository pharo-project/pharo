"
I return all abstract method from given scope. 
I select such methods using #isAbstract message. So it can be #subclassResponsibility methods or other trait related requirements.
"
Class {
	#name : #ClyAbstractMethodsQuery,
	#superclass : #ClyMethodQuery,
	#category : #'Calypso-SystemPlugins-InheritanceAnalysis-Queries'
}

{ #category : #printing }
ClyAbstractMethodsQuery >> description [

	^'abstract methods'
]

{ #category : #testing }
ClyAbstractMethodsQuery >> selectsMethod: aMethod [
	^aMethod sendsSelector: #subclassResponsibility
]
