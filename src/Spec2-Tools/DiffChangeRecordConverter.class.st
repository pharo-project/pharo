"
A DiffChangeRecordConverter is a converter used to show the diff between two methodreferences
"
Class {
	#name : #DiffChangeRecordConverter,
	#superclass : #AbstractMethodReferenceConverter,
	#category : #'Spec2-Tools-VersionBrowser'
}

{ #category : #private }
DiffChangeRecordConverter >> internalGetText [	

	^TextDiffBuilder
		 buildDisplayPatchFrom: ((self priorVersionOfAMethod: method) sourceCode) 
		to: (method sourceCode) 
		inClass: (method className)
]

{ #category : #private }
DiffChangeRecordConverter >> priorVersionOfAMethod: aChangeRecord [
	| index mylist |
	
	mylist := referencesList sorted: [:a :b | a timeStamp > b timeStamp ].
	index := mylist indexOf: aChangeRecord .
	^ mylist at: index+1 ifAbsent: [ aChangeRecord ]
]
