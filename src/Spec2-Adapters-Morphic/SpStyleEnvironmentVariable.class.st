"
A base for environment variables (values that point to values in the environment).
"
Class {
	#name : #SpStyleEnvironmentVariable,
	#superclass : #SpStyleAbstractVariable,
	#instVars : [
		'name'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #documentation }
SpStyleEnvironmentVariable class >> addDocumentExample: aBuilder [

	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Example' ] withLevel: 2.
	aBuilder newLine.
	aBuilder monospace: self documentExampleCode
]

{ #category : #documentation }
SpStyleEnvironmentVariable class >> addDocumentValidValues: aBuilder [	

	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Valid values' ] withLevel: 2.
	aBuilder newLine.
	aBuilder unorderedListDuring: [  
		(self documentValidValues sorted) do: [ :each |
			aBuilder item: [
				aBuilder monospace: '#', each asString ] ] ]	
]

{ #category : #documentation }
SpStyleEnvironmentVariable class >> buildMicroDownUsing: aBuilder withComment: aString [
	
	super buildMicroDownUsing: aBuilder withComment: aString.
	self addDocumentExample: aBuilder.
	self addDocumentValidValues: aBuilder	

]

{ #category : #documentation }
SpStyleEnvironmentVariable class >> documentExampleCode [

	^ '(none)'
]

{ #category : #documentation }
SpStyleEnvironmentVariable class >> documentValidValues [
	
	^ #()
]

{ #category : #'ston-core' }
SpStyleEnvironmentVariable class >> stonName [

	^ 'Environment'
]

{ #category : #'instance creation' }
SpStyleEnvironmentVariable >> fromSton: stonReader [

	self name: stonReader parseContainedValue
]

{ #category : #testing }
SpStyleEnvironmentVariable >> isEnvironmentVariable [

	^ true
]

{ #category : #accessing }
SpStyleEnvironmentVariable >> name [
	^ name
]

{ #category : #accessing }
SpStyleEnvironmentVariable >> name: anObject [
	name := anObject
]
