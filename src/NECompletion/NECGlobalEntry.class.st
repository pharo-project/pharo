"
I represent a global variable.
"
Class {
	#name : 'NECGlobalEntry',
	#superclass : 'NECVariableEntry',
	#category : 'NECompletion-Model',
	#package : 'NECompletion',
	#tag : 'Model'
}

{ #category : 'matching' }
NECGlobalEntry >> camelCasePartsOf: aString [

	| parts current |
	parts := OrderedCollection new.
	current := WriteStream on: String new.

	aString doWithIndex: [ :ch :index |
		(index > 1 and: [
			ch isUppercase and: [ current contents notEmpty ] ])
			ifTrue: [
				parts add: current contents.
				current := WriteStream on: String new ].
		current nextPut: ch ].

	current contents ifNotEmpty: [ parts add: current contents ].
	^ parts asArray
]

{ #category : 'accessing' }
NECGlobalEntry >> hightlightSymbol [
	^ #globalVar
]

{ #category : 'matching' }
NECGlobalEntry >> matchToken: token againstParts: parts [

	^ self
		matchToken: token
		againstParts: parts
		partIndex: 1
]

{ #category : 'matching' }
NECGlobalEntry >> matchToken: token againstParts: parts partIndex: startIndex [

	token ifEmpty: [ ^ true ].
	startIndex > parts size ifTrue: [ ^ false ].

	startIndex to: parts size do: [ :i |
		| part maxPrefix |
		part := parts at: i.
		maxPrefix := token size min: part size.

		1 to: maxPrefix do: [ :prefixSize |
			| piece |
			piece := part first: prefixSize.
			(token beginsWith: piece) ifTrue: [
				(self
					matchToken: (token allButFirst: prefixSize)
					againstParts: parts
					partIndex: i + 1) ifTrue: [ ^ true ] ] ] ].

	^ false
]

{ #category : 'matching' }
NECGlobalEntry >> matchesCamelCaseCompletionToken: aString caseSensitive: aBoolean [

	| token parts normalizedParts normalizedToken |
	aString ifEmpty: [ ^ true ].

	normalizedToken := aBoolean
		ifTrue: [ aString asString ]
		ifFalse: [ aString asString asLowercase ].

	parts := self camelCasePartsOf: self contents.
	parts isEmpty ifTrue: [ ^ false ].

	normalizedParts := aBoolean
		ifTrue: [ parts ]
		ifFalse: [ parts collect: [ :each | each asLowercase ] ].

	token := normalizedToken.
	^ self matchToken: token againstParts: normalizedParts
]

{ #category : 'accessing' }
NECGlobalEntry >> rawLabel [
	^ 'global'
]
