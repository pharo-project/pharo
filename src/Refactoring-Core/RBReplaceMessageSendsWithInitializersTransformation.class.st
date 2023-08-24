Class {
	#name : #RBReplaceMessageSendsWithInitializersTransformation,
	#superclass : #RBReplaceMessageSendTransformation,
	#instVars : [
		'newArgs'
	],
	#category : #'Refactoring-Core-Refactorings'
}

{ #category : #'instance creation' }
RBReplaceMessageSendsWithInitializersTransformation class >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: newSelector permutation: aMap newArgs: aCollection inAllClasses: aBoolean [

	^ self new
		  model: aRBSmalltalk
		  replaceMethod: aSelector
		  in: aClass
		  to: newSelector
		  permutation: aMap
		  inAllClasses: aBoolean
		  newArgs: aCollection;
		  yourself
]

{ #category : #initialization }
RBReplaceMessageSendsWithInitializersTransformation >> model: aRBSmalltalk replaceMethod: aSelector in: aClass to: aNewSelector permutation: aCollection inAllClasses: aBoolean newArgs: anArgsCollection [

	model := aRBSmalltalk.
	oldSelector := aSelector.
	class := aClass.
	newSelector := aNewSelector.
	permutation := aCollection.
	replaceInAllClasses := aBoolean.
	newArgs := anArgsCollection.
	initializers := anArgsCollection collect: [ :a | a value ]
]

{ #category : #private }
RBReplaceMessageSendsWithInitializersTransformation >> newSelectorString [
	| stream keywords indexNewArgs |
	stream := WriteStream on: String new.
	keywords := newSelector keywords.
	indexNewArgs := 0.
	permutation
		doWithIndex:
			[:each :index |
			stream nextPutAll: (keywords at: index).
			each < 0
				ifTrue:
					[ indexNewArgs := indexNewArgs - 1.
					stream
						nextPut: $(;
						nextPutAll: (newArgs at: indexNewArgs * -1) value1;
						nextPut: $)]
				ifFalse:
					[stream
						nextPutAll: ' ``@arg';
						nextPutAll: each printString].
			stream nextPut: $ ].
	^stream contents
]
