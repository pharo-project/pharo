"
A base for environment variables (values that point to values in the environment).
"
Class {
	#name : #SpStyleVariableEnvironment,
	#superclass : #SpStyleAbstractVariable,
	#instVars : [
		'name'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #documentation }
SpStyleVariableEnvironment class >> addDocumentExample: aBuilder [

	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Example' ] withLevel: 2.
	aBuilder newLine.
	aBuilder monospace: self documentExampleCode
]

{ #category : #documentation }
SpStyleVariableEnvironment class >> addDocumentValidValues: aBuilder [	

	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Valid values' ] withLevel: 2.
	aBuilder newLine.
	aBuilder unorderedListDuring: [  
		(self documentValidValues sorted) do: [ :each |
			aBuilder item: [
				aBuilder monospace: '#', each asString ] ] ]	
]

{ #category : #documentation }
SpStyleVariableEnvironment class >> buildMicroDownUsing: aBuilder withComment: aString [
	
	super buildMicroDownUsing: aBuilder withComment: aString.
	self addDocumentExample: aBuilder.
	self addDocumentValidValues: aBuilder	

]

{ #category : #documentation }
SpStyleVariableEnvironment class >> documentExampleCode [

	^ '(none)'
]

{ #category : #documentation }
SpStyleVariableEnvironment class >> documentValidValues [
	
	^ #()
]

{ #category : #'ston-core' }
SpStyleVariableEnvironment class >> stonName [

	^ 'Environment'
]

{ #category : #'instance creation' }
SpStyleVariableEnvironment >> fromSton: stonReader [

	self name: stonReader parseContainedValue
]

{ #category : #testing }
SpStyleVariableEnvironment >> isEnvironmentVariable [

	^ true
]

{ #category : #accessing }
SpStyleVariableEnvironment >> name [
	^ name
]

{ #category : #accessing }
SpStyleVariableEnvironment >> name: anObject [
	name := anObject
]
