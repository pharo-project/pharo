"
-- Regular Expression Matcher v 1.1 (C) 1996, 1999 Vassili Bykov
--
A generic syntax tree node, provides some common responses to the standard tests, as well as tree structure printing -- handy for debugging.
"
Class {
	#name : #RxsNode,
	#superclass : #Object,
	#category : #Regex-Core
}

{ #category : #constants }
RxsNode >> indentCharacter [
	"Normally, #printOn:withIndent: method in subclasses
	print several characters returned by this method to indicate
	the tree structure."

	^$+
]

{ #category : #testing }
RxsNode >> isAtomic [
	"Answer whether the node is atomic, i.e. matches exactly one 
	constant predefined normal character.  A matcher may decide to 
	optimize matching of a sequence of atomic nodes by glueing them 
	together in a string."

	^false "tentatively"
]

{ #category : #testing }
RxsNode >> isNullable [
	"True if the node can match an empty sequence of characters."

	^false "for most nodes"
]
