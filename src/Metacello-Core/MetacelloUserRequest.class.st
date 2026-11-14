"
I stand for whoever ran the script, as a requester: I ask for the roots of a load (see requestFor:). I am also the request of the findings about the load as a whole, such as a load with no root: I keep them, as a MetacelloProjectRequest keeps its own.
"
Class {
	#name : 'MetacelloUserRequest',
	#superclass : 'Object',
	#instVars : [
		'findings'
	],
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'findings' }
MetacelloUserRequest >> addFinding: aFinding [

	findings add: aFinding.
	^ aFinding
]

{ #category : 'requirements' }
MetacelloUserRequest >> appliesReplaces [
	"What I require is a root: the replaces of its baseline apply"

	^ true
]

{ #category : 'printing' }
MetacelloUserRequest >> chainDisplayNames [
	"The versions that led to a version the script requires: none, nobody asked before the script"

	^ #(  )
]

{ #category : 'printing' }
MetacelloUserRequest >> chainLine [
	"How the report says who asked, after 'Required by:': the script"

	^ self name
]

{ #category : 'printing' }
MetacelloUserRequest >> chainLines [
	"The versions that led to a version the script requires: none, nobody asked before the script"

	^ #(  )
]

{ #category : 'requests' }
MetacelloUserRequest >> chainNames [
	"The names of the projects that led to what the script asks for: none, nobody asked before the script"

	^ #(  )
]

{ #category : 'printing' }
MetacelloUserRequest >> displayName [
	"How the report names the user: the script"

	^ self name
]

{ #category : 'findings' }
MetacelloUserRequest >> findings [
	"What was found about the load as a whole"

	^ findings
]

{ #category : 'initialization' }
MetacelloUserRequest >> initialize [

	super initialize.
	findings := OrderedCollection new
]

{ #category : 'requirements' }
MetacelloUserRequest >> isAboutRepository: aRepository [
	"A finding about me is about the load as a whole, no repository"

	^ false
]

{ #category : 'requirements' }
MetacelloUserRequest >> isAboutSameRepositoryAs: aSubject [

	^ aSubject == self
]

{ #category : 'requirements' }
MetacelloUserRequest >> isSameVersionAs: aCandidateVersion [

	^ false
]

{ #category : 'requirements' }
MetacelloUserRequest >> isSelectedIn: aResolution [
	"The user is always heard"

	^ true
]

{ #category : 'requirements' }
MetacelloUserRequest >> isSupersededIn: aResolution [

	^ false
]

{ #category : 'printing' }
MetacelloUserRequest >> locate: aString [
	"A finding about the load as a whole is nowhere else: aString says it all"

	^ aString
]

{ #category : 'accessing' }
MetacelloUserRequest >> name [

	^ 'the script'
]

{ #category : 'printing' }
MetacelloUserRequest >> printOn: aStream [

	aStream nextPutAll: self name
]

{ #category : 'printing' }
MetacelloUserRequest >> printRequiredByOn: aStream [
	"A finding about the load as a whole: nobody but the script asked"
]

{ #category : 'requirements' }
MetacelloUserRequest >> request [
	"The request a finding about me is about: I am the request of the load as a whole"

	^ self
]

{ #category : 'requests' }
MetacelloUserRequest >> requestFor: aProjectSpec [
	"My request for the root aProjectSpec names"

	^ MetacelloProjectRequest requester: self projectSpec: aProjectSpec
]

{ #category : 'requirements' }
MetacelloUserRequest >> requiresInstalledVersions [
	"What I require, a root, is not installed by being required"

	^ false
]
