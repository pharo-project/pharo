"
A LiteralDictionary, like an IdentityDictionary, has a special test for equality.  In this case it is simple equality between objects of like class.  This allows equal Float or String literals to be shared without the possibility of erroneously sharing, say, 1 and 1.0
"
Class {
	#name : #LiteralDictionary,
	#superclass : #Dictionary,
	#category : 'Compiler-Support'
}

{ #category : #testing }
LiteralDictionary >> literalEquality: x and: y [
	"Check if two literals should be considered equal and reduced to a single literal.
	Delegate this task to the literal themselves, they are aware of their peculiarities and know how to behave."
	
	^ x literalEqual: y

]

{ #category : #internal }
LiteralDictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| element start finish |
	finish := array size.
	start := (anObject hash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil
					or: [self literalEquality: element key and: anObject])
					ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil
					or: [self literalEquality: element key and: anObject])
					ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]
