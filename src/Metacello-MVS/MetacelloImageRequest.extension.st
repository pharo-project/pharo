Extension { #name : 'MetacelloImageRequest' }

{ #category : '*Metacello-MVS' }
MetacelloImageRequest >> appliesReplaces [
	"Only the replaces of a root apply"

	^ false
]

{ #category : '*Metacello-MVS' }
MetacelloImageRequest >> chainDisplayNames [
	"The versions that led to a version the image requires: none, nobody asked before the image"

	^ #(  )
]

{ #category : '*Metacello-MVS' }
MetacelloImageRequest >> chainLine [
	"How the report says who asked, after 'Required by:': the image"

	^ self name
]

{ #category : '*Metacello-MVS' }
MetacelloImageRequest >> chainLines [
	"The versions that led to a version the image requires: none, the image has it"

	^ #(  )
]

{ #category : '*Metacello-MVS' }
MetacelloImageRequest >> displayName [
	"How the report names the image: the image"

	^ self name
]

{ #category : '*Metacello-MVS' }
MetacelloImageRequest >> requiresInstalledVersions [

	^ true
]
