Class {
	#name : #GLMSingleSpotterRequest,
	#superclass : #Notification,
	#instVars : [
		'prompt',
		'default',
		'assisted',
		'searchBlock',
		'labelBlock',
		'iconBlock'
	],
	#category : 'Glamour-Morphic-Renderer-Spotter'
}

{ #category : #utilities }
GLMSingleSpotterRequest class >> substring: testString matches: aString caseSensitive: aBoolean [
	"Checks if a testString is a substring of aString. The matching parts do not necessarily need to be consecutive, for example 'egli' matches 'renggli'."

	| index |
	index := 0.
	testString do: [ :char |
		index := aString
			findString: (String with: char)
			startingAt: index + 1
			caseSensitive: aBoolean.
		index = 0
			ifTrue: [ ^ false ] ].
	^ true
]

{ #category : #accessing }
GLMSingleSpotterRequest >> assisted [
	^ assisted
]

{ #category : #accessing }
GLMSingleSpotterRequest >> assisted: aBoolean [
	"A boolean indicating that the typing of the user is assisted but not constrained."
	
	assisted := aBoolean
]

{ #category : #configuration }
GLMSingleSpotterRequest >> collection: aCollection [
	self collection: aCollection caseSensitive: false
]

{ #category : #configuration }
GLMSingleSpotterRequest >> collection: aCollection caseSensitive: aBoolean [
	"Configure this completion dialog with aCollection and match the elements case sensitive if aBoolean is true. Display the complete list if the filter is empty."

	self searchBlock: [ :value |
		value isEmpty
			ifTrue: [ aCollection ]
			ifFalse: [
				aCollection select: [ :each |
					self class
						substring: value
						matches: (self labelFor: each)
						caseSensitive: aBoolean ] ] ]
]

{ #category : #accessing }
GLMSingleSpotterRequest >> default [
	^ default
]

{ #category : #accessing }
GLMSingleSpotterRequest >> default: aString [
	"The default string initially used for filtering."
	
	default := aString
]

{ #category : #initialize-release }
GLMSingleSpotterRequest >> defaultAction [
	^(GLMMorphicSingleSpotter openOn: self) answer
]

{ #category : #dispatching }
GLMSingleSpotterRequest >> handleWith: anObject [
	^ anObject handleCompletionRequest: self
]

{ #category : #accessing }
GLMSingleSpotterRequest >> iconBlock [
	^ iconBlock
]

{ #category : #accessing }
GLMSingleSpotterRequest >> iconBlock: aOneArgumentBlock [
	"A one argument block answering the icon symbol for a given element."
	
	iconBlock := aOneArgumentBlock
]

{ #category : #querying }
GLMSingleSpotterRequest >> iconFor: anObject [
	^ iconBlock value: anObject
]

{ #category : #initialization }
GLMSingleSpotterRequest >> initialize [
	super initialize.
	prompt := 'Completion Request'.
	default := String new.
	assisted := false.
	searchBlock := [ :value | #() ].
	labelBlock := [ :value | value asString ].
	iconBlock := [ :value | nil ]
]

{ #category : #accessing }
GLMSingleSpotterRequest >> labelBlock [
	^ labelBlock
]

{ #category : #accessing }
GLMSingleSpotterRequest >> labelBlock: aOneArgumentBlock [
	"A one argument block answering the string for a given element."
	
	labelBlock := aOneArgumentBlock
]

{ #category : #querying }
GLMSingleSpotterRequest >> labelFor: anObject [
	^ labelBlock value: anObject
]

{ #category : #accessing }
GLMSingleSpotterRequest >> name [
	^self prompt
]

{ #category : #accessing }
GLMSingleSpotterRequest >> prompt [
	^ prompt
]

{ #category : #accessing }
GLMSingleSpotterRequest >> prompt: aString [
	"A string with the title for this request."
	
	prompt := aString
]

{ #category : #accessing }
GLMSingleSpotterRequest >> searchBlock [
	^ searchBlock
]

{ #category : #accessing }
GLMSingleSpotterRequest >> searchBlock: aOneArgumentBlock [
	"A one argument block returning the elements for the given filter."
	
	searchBlock := aOneArgumentBlock
]

{ #category : #querying }
GLMSingleSpotterRequest >> valuesFor: anObject [
	^ searchBlock value: anObject
]
