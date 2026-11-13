Extension { #name : 'MetacelloImageRequest' }

{ #category : '*Metacello-MVS' }
MetacelloImageRequest >> appliesReplaces [
	"Only the replaces of a root apply"

	^ false
]

{ #category : '*Metacello-MVS' }
MetacelloImageRequest >> chainLines [
	"The versions that led to a version I require: none, the image has it"

	^ OrderedCollection new
]

{ #category : '*Metacello-MVS' }
MetacelloImageRequest >> requiresInstalledVersions [

	^ true
]
