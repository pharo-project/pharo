"
The method name/selector of keyword messages should use camel case. So every keyword part of the keyword message should start with a lowercase letter.
"
Class {
	#name : 'ReMethodSelectorKeywordCasingRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Design Flaws',
	#package : 'General-Rules',
	#tag : 'Design Flaws'
}

{ #category : 'testing - interest' }
ReMethodSelectorKeywordCasingRule class >> checksMethod [

	^ true
]

{ #category : 'accessing' }
ReMethodSelectorKeywordCasingRule class >> group [

	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReMethodSelectorKeywordCasingRule class >> ruleName [
	^ 'Keyword selector has wrong casing (camel case required)'
]

{ #category : 'manifest' }
ReMethodSelectorKeywordCasingRule class >> uniqueIdentifierName [
	"The return value hould be unique and should change only when the rule completely change semantics"

	^'MethodSelectorKeywordCasingRule'
]

{ #category : 'running' }
ReMethodSelectorKeywordCasingRule >> basicCheck: aMethod [
 
	^ aMethod selector isKeyword and: [
		  (aMethod selector asString substrings: #( $: )) anySatisfy: [
			  :each | each first isUppercase ] ]
]
