"
I am a query of unclassified classes.

I a bit complex class query which returns classes without any tag.
The complexity is related to the logic that I will return nothing from the package which has no tags at all.

You can see class group #extensions which is based on me. 
It appears only when tagged and untagged classes exist together in the package
"
Class {
	#name : #ClyRestUntaggedClassesQuery,
	#superclass : #ClyClassQuery,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #execution }
ClyRestUntaggedClassesQuery >> buildResult: aQueryResult [
	
	| foundClasses |
	scope packagesDo: [ :each | 
		each tagsForClasses ifNotEmpty: [ 
			foundClasses := scope query: ClyUntaggedClassesQuery new.
			^aQueryResult fillWith: foundClasses rawItems]]
]

{ #category : #execution }
ClyRestUntaggedClassesQuery >> checkEmptyResult [
	
	scope packagesDo: [ :each | 
		each tagsForClasses ifNotEmpty: [ 
			^scope isQueryEmpty: ClyUntaggedClassesQuery new]].
	
	^true
]

{ #category : #printing }
ClyRestUntaggedClassesQuery >> description [

	^'rest unclassified classes'
]

{ #category : #testing }
ClyRestUntaggedClassesQuery >> selectsClass: aClass [
	^aClass tags isEmpty
]
