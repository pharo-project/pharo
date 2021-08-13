"
I represent a group of rules in CriticBrowser tree view
"
Class {
	#name : #CriticBrowserRulesGroup,
	#superclass : #Object,
	#instVars : [
		'name',
		'rules'
	],
	#category : #'Tool-CriticBrowser-Utilities'
}

{ #category : #'instance creation' }
CriticBrowserRulesGroup class >> isDeprecated [ 
	"We do not need this class anymore. Use CBCriticBrowserRulesGroup instead" 

	^true
]

{ #category : #'instance creation' }
CriticBrowserRulesGroup class >> named: aString rules: aCollection [
	"please use CBCritiqueBrowserRulesGroup>>named: @arg rules: @arg"	
	self deprecated: 'use CBCritiqueBrowserRulesGroup>>named: @arg rules: @arg'.

	^ self new
		name: aString;
		rules: aCollection;
		yourself
]

{ #category : #enumerating }
CriticBrowserRulesGroup >> do: aBlock [

	rules do: aBlock
]

{ #category : #testing }
CriticBrowserRulesGroup >> isComposite [

	^ true
]

{ #category : #accessing }
CriticBrowserRulesGroup >> name [
	^ name
]

{ #category : #accessing }
CriticBrowserRulesGroup >> name: anObject [
	name := anObject
]

{ #category : #accessing }
CriticBrowserRulesGroup >> rules [
	^ rules
]

{ #category : #accessing }
CriticBrowserRulesGroup >> rules: anObject [
	rules := anObject
]
