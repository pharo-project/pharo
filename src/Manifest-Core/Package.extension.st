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

	^ self packageManifest ifNil: [ '' ] ifNotNil: [ :manifest |
		  manifest comment: aDescription stamp: Date today mmddyyyy , ' '
			  ,
				  ((String streamContents: [ :s | Time now print24: true on: s ])
					   copyFrom: 1
					   to: 5) ]
]

{ #category : '*Manifest-Core' }
Package >> packageManifest [
	^ self definedClasses
		detect: [ :each | each isManifest ]
		ifNone: [ TheManifestBuilder new createManifestNamed: self name]
]
