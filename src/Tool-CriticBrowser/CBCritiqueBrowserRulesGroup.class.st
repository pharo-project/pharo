"
I represent a group of rules in CBCritiqueBrowser tree view
"
Class {
	#name : #CBCritiqueBrowserRulesGroup,
	#superclass : #Object,
	#instVars : [
		'name',
		'rules'
	],
	#category : #'Tool-CriticBrowser-Utilities'
}

{ #category : #'instance creation' }
CBCritiqueBrowserRulesGroup class >> named: aString rules: aCollection [
	^ self new
		  name: aString;
		  rules: (aCollection sorted: [ :a :b | a name < b name ]);
		  yourself
]

{ #category : #enumerating }
CBCritiqueBrowserRulesGroup >> do: aBlock [

	rules do: aBlock
]

{ #category : #testing }
CBCritiqueBrowserRulesGroup >> isComposite [

	^ true
]

{ #category : #accessing }
CBCritiqueBrowserRulesGroup >> name [
	^ name
]

{ #category : #accessing }
CBCritiqueBrowserRulesGroup >> name: anObject [
	name := anObject
]

{ #category : #accessing }
CBCritiqueBrowserRulesGroup >> rules [
	^ rules
]

{ #category : #accessing }
CBCritiqueBrowserRulesGroup >> rules: anObject [
	rules := anObject
]
