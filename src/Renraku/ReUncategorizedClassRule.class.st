"
Check if the class is uncategorized within its package - if yes it should critisized so it will be tagged
with either a proper tag or a tag called ""Base"" as then the class belongs to the base part of the package.
"
Class {
	#name : #ReUncategorizedClassRule,
	#superclass : #ReAbstractRule,
	#classVars : [
		'DependencyChecker'
	],
	#category : #'Renraku-Rules'
}

{ #category : #testing }
ReUncategorizedClassRule class >> checksClass [ 

	^true
]

{ #category : #accessing }
ReUncategorizedClassRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"
	
	^'UncategorizedClassRule'
]

{ #category : #running }
ReUncategorizedClassRule >> basicCheck: aClass [

	^(aClass category = aClass package name) 
]

{ #category : #accessing }
ReUncategorizedClassRule >> group [
	^ 'Coding Idiom Violation'
]

{ #category : #accessing }
ReUncategorizedClassRule >> name [
	"Answer a human readable name of the rule."

	^'Categorize class within package with a proper tag or tag with "Base".'
]

{ #category : #accessing }
ReUncategorizedClassRule >> rationale [
	"Answer a human readable name of the rule."

	^'Categorize class within package with a proper tag or tag with "Base" (to show class belongs to the base functionality of the package). This helps having a well structured package.'
]
