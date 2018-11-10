"
I am a query of unclassified classes.

I a bit complex class query which returns classes without any tag.
The complexity is related to the logic that I will return nothing from the package which has no tags at all.

You can see class group #extensions which is based on me. 
It appears only when tagged and untagged classes exist together in the package
"
Class {
	#name : #ClyRestUntaggedClasses,
	#superclass : #ClyClassQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #execution }
ClyRestUntaggedClasses >> buildResult: aQueryResult [
	
	| foundClasses |
	scope packagesDo: [ :each | 
		each tagsForClasses ifNotEmpty: [ 
			foundClasses := scope query: ClyUntaggedClasses new.
			^aQueryResult fillWith: foundClasses rawItems]]
]

{ #category : #execution }
ClyRestUntaggedClasses >> checkEmptyResult [
	
	scope packagesDo: [ :each | 
		each tagsForClasses ifNotEmpty: [ 
			^scope isQueryEmpty: ClyUntaggedClasses new]].
	
	^true
]

{ #category : #printing }
ClyRestUntaggedClasses >> description [

	^'rest unclassified classes'
]

{ #category : #testing }
ClyRestUntaggedClasses >> selectsClass: aClass [
	^aClass tags isEmpty
]
