"
This is the particular englobing node when the first message of a cascade node is faulty.

E.g. `5;foo` or `(5 foo);bar` where there is a value node (a possible receiver) but no message.
The only content of the error node is this possible receiver.

It is also used if the first message is empty: not even a receiver, e.g. `;foo`.
In this case the content (the receiver) is an error node (expected value).

Beware that instances of this class can be the first element of ""messages"" on a faulty RBCascadeNode object.
"
Class {
	#name : #RBInvalidCascadeErrorNode,
	#superclass : #RBEnglobingErrorNode,
	#category : #'AST-Core-Nodes - ErrorNodes'
}

{ #category : #testing }
RBInvalidCascadeErrorNode >> isCascadeError [
	^true
]

{ #category : #accessing }
RBInvalidCascadeErrorNode >> receiver [
	^ contents first
]
