"
I am special version of a dictionary that compares the keys using the #literalEquals: message.
"
Class {
	#name : 'OCLiteralDictionary',
	#superclass : 'Dictionary',
	#category : 'OpalCompiler-Core-DataStructures',
	#package : 'OpalCompiler-Core',
	#tag : 'DataStructures'
}

{ #category : 'accessing' }
OCLiteralDictionary >> keyAtValue: value ifAbsent: exceptionBlock [
	"Answer the key that is the external name for the argument, value. If
	there is none, answer the result of evaluating exceptionBlock."

	self associationsDo:
		[:association | (value literalEqual: association value) ifTrue: [^ association key]].
	^ exceptionBlock value
]

{ #category : 'private' }
OCLiteralDictionary >> scanFor: anObject [
	"Scan the key array for the first slot containing either a nil (indicating an empty slot) or an element that matches anObject. Answer the index of that slot or zero if no slot is found. This method will be overridden in various subclasses that have different interpretations for matching elements."
	| finish start element |
	finish := array size.
	start := (anObject hash \\ finish) + 1.

	"Search from (hash mod size) to the end."
	start to: finish do:
		[:index | ((element := array at: index) == nil or: [element key literalEqual: anObject])
			ifTrue: [^ index ]].

	"Search from 1 to where we started."
	1 to: start-1 do:
		[:index | ((element := array at: index) == nil or: [element key literalEqual: anObject])
			ifTrue: [^ index ]].

	^ 0  "No match AND no empty slot"
]
