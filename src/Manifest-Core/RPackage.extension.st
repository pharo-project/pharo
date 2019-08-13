Extension { #name : #RPackage }

{ #category : #'*Manifest-Core' }
RPackage >> criticNameOn: aStream [
	"This behavior may be folded later by changing the name of this method or using another one."
	
	aStream << self packageName 
]

{ #category : #'*Manifest-Core' }
RPackage >> manifestBuilderForRuleChecker: aRuleChecker [
	"Return the manifestsince the rulechecker is keeping a cache, we ask it back"

	^ aRuleChecker manifestBuilderOfPackage: self
]

{ #category : #'*Manifest-Core' }
RPackage >> packageComment [
	^ self packageManifestOrNil ifNil: [ '' ] ifNotNil: [ :manifest | manifest comment ]
]

{ #category : #'*Manifest-Core' }
RPackage >> packageComment: aDescription [
	^ self packageManifest
		ifNil: [ '' ]
		ifNotNil: [ :manifest | manifest comment: aDescription stamp: Author changeStamp]
]

{ #category : #'*Manifest-Core' }
RPackage >> packageManifest [
	^ self definedClasses
		detect: [ :each | each isManifest ]
		ifNone: [ TheManifestBuilder new createManifestNamed: name]
]
