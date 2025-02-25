Extension { #name : 'PackageTag' }

{ #category : '*Manifest-Core' }
PackageTag >> manifestBuilderForRuleChecker: aRuleChecker [
	"Return the manifestsince the rulechecker is keeping a cache, we ask it back"

	^ aRuleChecker manifestBuilderOfPackage: self package
]
