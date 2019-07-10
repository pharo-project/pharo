"
I represent first class severity levels of critiques.
There are three of them: information, warning, error. 

You can get me from critique using: 
	
	ClyCrituqueSeverity of: aCritiques.
	
I provide name and color for the UI. And I allow to sort critiques by severity using my method #isMoreImportantThan:. 

Internal Representation and Key Implementation Points.

    Instance Variables
	color:		<Color>
	name:		<String>
"
Class {
	#name : #ClyCritiqueSeverityProperty,
	#superclass : #ClyProperty,
	#instVars : [
		'name',
		'color'
	],
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #'instance creation' }
ClyCritiqueSeverityProperty class >> default [
	^self new 
		name: #information;
		color: Color blue
]

{ #category : #'instance creation' }
ClyCritiqueSeverityProperty class >> of: aCritique [

	^self new 
		name: aCritique rule severity;
		color: aCritique color
]

{ #category : #accessing }
ClyCritiqueSeverityProperty >> color [
	^ color
]

{ #category : #accessing }
ClyCritiqueSeverityProperty >> color: anObject [
	color := anObject
]

{ #category : #accessing }
ClyCritiqueSeverityProperty >> iconName [
	^ 'small', name capitalized, 'Icon'
]

{ #category : #testing }
ClyCritiqueSeverityProperty >> isMoreImportantThan: anotherSeverity [

	^self level > anotherSeverity level
]

{ #category : #accessing }
ClyCritiqueSeverityProperty >> level [
	^ #(information warning error) indexOf: name
]

{ #category : #accessing }
ClyCritiqueSeverityProperty >> name [
	^ name
]

{ #category : #accessing }
ClyCritiqueSeverityProperty >> name: anObject [
	name := anObject
]
