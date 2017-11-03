"
This announcement will be emitted when a class or a trait definition changes.

For example, when an instance variable is added or when the trait composition is modified in a class definition.
"
Class {
	#name : #ClassModifiedClassDefinition,
	#superclass : #ClassAnnouncement,
	#instVars : [
		'newClassDefinition',
		'oldClassDefinition'
	],
	#category : #System-Announcements-System-Classes
}

{ #category : #'instance creation' }
ClassModifiedClassDefinition class >> classDefinitionChangedFrom: oldClass to: newClass [
	^self new
			oldClassDefinition: oldClass;
			newClassDefinition: newClass;
			yourself
]

{ #category : #accessing }
ClassModifiedClassDefinition >> classAffected [
	^self newClassDefinition
]

{ #category : #accessing }
ClassModifiedClassDefinition >> newClassDefinition [
	^newClassDefinition
]

{ #category : #accessing }
ClassModifiedClassDefinition >> newClassDefinition: aClass [
	newClassDefinition := aClass
]

{ #category : #accessing }
ClassModifiedClassDefinition >> oldClassDefinition [
	^oldClassDefinition
]

{ #category : #accessing }
ClassModifiedClassDefinition >> oldClassDefinition: aClass [
	oldClassDefinition := aClass
]
