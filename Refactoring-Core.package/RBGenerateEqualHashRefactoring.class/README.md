I am a refactoring for generating #hash and #= comparing methods.

For example, a Class with three instance methods inst1-inst3

RBGenerateEqualHashRefactoring model:RBNamespace new className: ClassS variables: { #inst1 . #inst2 . #inst3 }.

will create:
a #hash method 
hash
	"Answer an integer value that is related to the identity of the receiver."

	^ inst1 hash bitXor: (inst2 hash bitXor: inst3 hash)
	
and a #= method
= anObject
	"Answer whether the receiver and anObject represent the same object."

	self == anObject
		ifTrue: [ ^ true ].
	self class = anObject class
		ifFalse: [ ^ false ].
	^ inst1 = anObject inst1
		and: [ inst2 = anObject inst2 and: [ inst3 = anObject inst3 ] ]

and any instvar accessor for the  instance variables used by method #=.
