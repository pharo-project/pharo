Extension { #name : 'Package' }

{ #category : '*Manifest-Core' }
Package >> manifestBuilderForRuleChecker: aRuleChecker [
	"Return the manifestsince the rulechecker is keeping a cache, we ask it back"

	^ aRuleChecker manifestBuilderOfPackage: self
]

{ #category : '*Manifest-Core' }
Package >> packageComment [
	^ self packageManifestOrNil ifNil: [ '' ] ifNotNil: [ :manifest | manifest comment ]
]

{ #category : '*Manifest-Core' }
Package >> packageComment: aDescription [
	^ self packageManifest
		ifNil: [ '' ]
		ifNotNil: [ :manifest | manifest comment: aDescription stamp: Author changeStamp]
]

{ #category : '*Manifest-Core' }
Package >> packageManifest [

	^ TheManifestBuilder ofPackage: self
]
