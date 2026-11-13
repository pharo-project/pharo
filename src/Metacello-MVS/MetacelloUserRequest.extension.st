Extension { #name : 'MetacelloUserRequest' }

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> appliesReplaces [
	"What I require is a root: the replaces of its baseline apply"

	^ true
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> chainDisplayNames [
	"The versions that led to a version the script requires: none, nobody asked before the script"

	^ #(  )
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> chainLine [
	"Who asked, for people"

	^ self name
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> chainLines [
	"The versions that led to a version the script requires: none, nobody asked before the script"

	^ #(  )
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> displayName [
	"For people"

	^ self name
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> isAboutRepository: aRepository [
	"A finding about me is about the load as a whole, no repository"

	^ false
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> isAboutSameRepositoryAs: aSubject [

	^ aSubject == self
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> isSameVersionAs: aCandidateVersion [

	^ false
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> isSelectedIn: aResolution [
	"The user is always heard"

	^ true
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> isSupersededIn: aResolution [

	^ false
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> locate: aString [
	"A finding about the load as a whole is nowhere else: aString says it all"

	^ aString
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> printRequiredByOn: aStream [
	"A finding about the load as a whole: nobody but the script asked"
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> request [
	"The request a finding about me is about: I am the request of the load as a whole"

	^ self
]

{ #category : '*Metacello-MVS' }
MetacelloUserRequest >> requiresInstalledVersions [
	"What I require, a root, is not installed by being required"

	^ false
]
