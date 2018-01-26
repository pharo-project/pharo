"
I'm used when completing a method override. I contain all selectors of the superclass minus the already implemented selectors of the current class. When a completion occurs I complete a method template with a send to super.
"
Class {
	#name : #NECOverrideModel,
	#superclass : #NECTypedModel,
	#category : #'NECompletion-Model'
}

{ #category : #action }
NECOverrideModel >> completionAt: aNumber [ 
	| output source declaration |
	source :=  (self methodAt: aNumber) sourceCode.
	declaration := (source lineCorrespondingToIndex: 1) trimRight.
	output := WriteStream on: String new.
	output nextPutAll: declaration; cr; tab.
	(source includesSubstring: 'subclassResponsibility') ifFalse: [
		(source includes: $^)
			ifTrue: [ output nextPutAll: '^ ' ].
		output
			nextPutAll: 'super ';
			nextPutAll: declaration ].
	^ output contents
]

{ #category : #'initialize-release' }
NECOverrideModel >> initializeSelectors [
	clazz superclass ifNotNil: [ self initializeSelectorsFor: clazz superclass ].
	clazz
		selectorsDo: [ :each | selectors detect: [ :ea | ea contentsAsSymbol == each ] ifFound: [ :entry | selectors remove: entry ifAbsent: [  ] ] ]
]

{ #category : #private }
NECOverrideModel >> methodAt: aNumber [ 
	^ clazz lookupSelector: (self at: aNumber) contentsAsSymbol
]

{ #category : #action }
NECOverrideModel >> title [
	^ '(override) ' , 
            (clazz superclass ifNotNil: #name ifNil: [ clazz name ])
]
