"
I am posible result of class query.
I convert given clases to bindings.

I am used by ClyClassReferences query to adopt class queries to the variables (bindings are polymorphic to them)
"
Class {
	#name : #ClyClassBindings,
	#superclass : #ClyQueryResult,
	#category : #'Calypso-SystemQueries-Results'
}

{ #category : #building }
ClyClassBindings >> fillWith: classes [
	items := classes collect: [ :each |
		(each isKindOf: Association) 
			ifTrue: [ each ] 
			ifFalse: [ 
				each isObsolete 
					ifTrue: [ GlobalVariable 
						key: (each name allButFirst: 'AnObsolete' size) asSymbol 
						value: each]
					ifFalse: [ each instanceSide binding ]]]

]
