"
All methods should be put into a protocol (method category) for better readability.
"
Class {
	#name : 'ReUnclassifiedMethodsRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Style',
	#package : 'General-Rules',
	#tag : 'Style'
}

{ #category : 'testing' }
ReUnclassifiedMethodsRule class >> checksMethod [
	^ true
]

{ #category : 'accessing' }
ReUnclassifiedMethodsRule class >> group [
	^ 'Style'
]

{ #category : 'accessing' }
ReUnclassifiedMethodsRule class >> ruleName [
	^ 'Unclassified methods'
]

{ #category : 'accessing' }
ReUnclassifiedMethodsRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^'UnclassifiedMethodsRule'
]

{ #category : 'running' }
ReUnclassifiedMethodsRule >> basicCheck: aMethod [

	^ aMethod isClassified not
]
