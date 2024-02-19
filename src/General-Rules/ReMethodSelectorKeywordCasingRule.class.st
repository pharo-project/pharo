"
The method name/selector of keyword messages should use camel case. So every keyword part of the keyword message should start with a lowercase letter.
"
Class {
	#name : 'ReMethodSelectorKeywordCasingRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Migrated',
	#package : 'General-Rules',
	#tag : 'Migrated'
}

{ #category : 'testing - interest' }
ReMethodSelectorKeywordCasingRule class >> checksMethod [

	^ true
]

{ #category : 'running' }
ReMethodSelectorKeywordCasingRule >> basicCheck: aMethod [
 
	^ aMethod selector isKeyword and: [
		  (aMethod selector asString substrings: #( $: )) anySatisfy: [
			  :each | each first isUppercase ] ]
]

{ #category : 'accessing' }
ReMethodSelectorKeywordCasingRule >> group [
	^ 'Design Flaws'
]

{ #category : 'accessing' }
ReMethodSelectorKeywordCasingRule >> name [
	^ 'Keyword selector has wrong casing'
]
