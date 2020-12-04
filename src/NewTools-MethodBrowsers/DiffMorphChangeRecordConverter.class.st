"
A DiffMorphChangeRecordConverter is a converter used to show the diff between two methodreferences with a side by side view.
"
Class {
	#name : #DiffMorphChangeRecordConverter,
	#superclass : #AbstractMethodReferenceConverter,
	#category : #'NewTools-MethodBrowsers-Base'
}

{ #category : #private }
DiffMorphChangeRecordConverter >> internalGetText [
	| priorSource |
	priorSource := (self priorVersionOfAMethod: method) ifNotNil: [ :m | m sourceCode ] ifNil: [ '' ].
	^ Array with: priorSource with: method sourceCode
]

{ #category : #private }
DiffMorphChangeRecordConverter >> priorVersionOfAMethod: aChangeRecord [
	| index |
	index := referencesList identityIndexOf: aChangeRecord .
	^ referencesList at: index+1 ifAbsent: [ nil ]
]
