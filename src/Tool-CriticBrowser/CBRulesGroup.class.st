"
I represent a group of rules in CriticBrowser tree view
"
Class {
	#name : #CBRulesGroup,
	#superclass : #Object,
	#instVars : [
		'name',
		'rules'
	],
	#category : #'Tool-CriticBrowser-Utilities'
}

{ #category : #'instance creation' }
CBRulesGroup class >> named: aString rules: aCollection [

	^ self new
		name: aString;
		rules: aCollection;
		yourself
]

{ #category : #'as yet unclassified' }
CBRulesGroup >> CBCommandExecuteOn: aCBCommand [
	self rules do: [ :each | aCBCommand context reapplyRule: each ]
]

{ #category : #enumerating }
CBRulesGroup >> do: aBlock [

	rules do: aBlock
]

{ #category : #testing }
CBRulesGroup >> isComposite [

	^ true
]

{ #category : #accessing }
CBRulesGroup >> name [
	^ name
]

{ #category : #accessing }
CBRulesGroup >> name: anObject [
	name := anObject
]

{ #category : #accessing }
CBRulesGroup >> rules [
	^ rules
]

{ #category : #accessing }
CBRulesGroup >> rules: anObject [
	rules := anObject
]
