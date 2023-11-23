Extension { #name : 'Object' }

{ #category : '*Tool-Finder' }
Object class >> approvedSelectorsForMethodFinder [

	^ #(at: basicAt: basicSize yourself size
	    ->
	    = == ~= hash literalEqual
	   )
]

{ #category : '*Tool-Finder' }
Object >> closeTo: anObject [
	"Do not use this Method! It is a workaround for MethodFinder"

	^[self = anObject] onErrorDo: [false]
]
