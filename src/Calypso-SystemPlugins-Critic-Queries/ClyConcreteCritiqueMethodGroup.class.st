"
I am a method group which include methods with critiques of particular severity level.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	ruleSeverity:		<ClyCritiqueSeverity>
"
Class {
	#name : #ClyConcreteCritiqueMethodGroup,
	#superclass : #ClyMethodGroup,
	#instVars : [
		'ruleSeverity'
	],
	#category : #'Calypso-SystemPlugins-Critic-Queries'
}

{ #category : #'instance creation' }
ClyConcreteCritiqueMethodGroup class >> basedOn: aCritique withMethodsFrom: aScope [

	| groupName methodQuery |
	groupName := aCritique rule group.
	methodQuery := ClyAllProblemMethodQuery 
			from: aScope withCritiquesFromGroup: groupName.

	^self named: groupName asLowercase on: methodQuery
]

{ #category : #accessing }
ClyConcreteCritiqueMethodGroup >> addCritique: aCritique [
	| newSeverity |
	newSeverity := ClyCritiqueSeverityProperty of: aCritique.
	(newSeverity isMoreImportantThan: ruleSeverity)
		ifTrue: [ ruleSeverity := newSeverity ]
]

{ #category : #initialization }
ClyConcreteCritiqueMethodGroup >> initialize [
	super initialize.
	
	ruleSeverity := ClyCritiqueSeverityProperty default
]

{ #category : #accessing }
ClyConcreteCritiqueMethodGroup >> ruleSeverity [
	^ ruleSeverity
]

{ #category : #accessing }
ClyConcreteCritiqueMethodGroup >> ruleSeverity: anObject [
	ruleSeverity := anObject
]
