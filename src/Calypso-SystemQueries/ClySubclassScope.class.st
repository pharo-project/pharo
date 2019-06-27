"
I represent scope of all subclasses of my basis classes.  
I show all subclasses, their methods and variables
"
Class {
	#name : #ClySubclassScope,
	#superclass : #ClySingleClassRelationScope,
	#category : #'Calypso-SystemQueries-Scopes'
}

{ #category : #accessing }
ClySubclassScope class >> classesRelatedTo: aClass do: aBlock [

	aClass subclassesDo: aBlock
]

{ #category : #accessing }
ClySubclassScope class >> defaultName [
	^'subclasses'
]
