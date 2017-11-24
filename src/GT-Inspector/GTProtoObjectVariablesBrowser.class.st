"
This is a specialization that works with ProtoObject and all its subclasses that except for Object that has its own specialization.

Example:
self openOn: ProtoObject new
"
Class {
	#name : #GTProtoObjectVariablesBrowser,
	#superclass : #GTRawObjectVariablesBrowser,
	#category : #'GT-Inspector'
}

{ #category : #'private building' }
GTProtoObjectVariablesBrowser >> basicVariableNodesFor: anObject in: aCollection [

	aCollection addAll: ((anObject class allInstVarNames 
			collectWithIndex: [ :ivName :index | 
				GTInspectorProtoObjectNode hostObject: anObject index: index slotName: ivName]) 
					sort: [:aNode :anotherNode | aNode label < anotherNode label ])
			
]

{ #category : #'private building' }
GTProtoObjectVariablesBrowser >> printStringOf: anObject [
	| className |
	className := anObject class name.
	^ (String streamContents: [ :s |
		s
			nextPutAll: (className first isVowel ifTrue: ['an '] ifFalse: ['a ']);
			nextPutAll: className ]) asComment
]
