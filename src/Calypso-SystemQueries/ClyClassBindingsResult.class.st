"
I am posible result of class query.
I convert given clases to bindings.

I am used by ClyClassReferences query to adopt class queries to the variables (bindings are polymorphic to them)
"
Class {
	#name : #ClyClassBindingsResult,
	#superclass : #ClyQueryResult,
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #building }
ClyClassBindingsResult >> fillWith: classes [
	items := classes collect: [ :each |
		each isVariableBinding 
			ifTrue: [ each ] 
			ifFalse: [ 
				each isObsolete 
					ifTrue: [ GlobalVariable 
						key: (each name allButFirst: 'AnObsolete' size) asSymbol 
						value: each]
					ifFalse: [ each instanceSide binding ]]]

]
