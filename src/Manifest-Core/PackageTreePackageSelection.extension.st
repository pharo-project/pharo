Extension { #name : #PackageTreePackageSelection }

{ #category : #'*Manifest-Core' }
PackageTreePackageSelection >> manifestBuilderForRuleChecker: aRuleChecker [
	"Return the manifestsince the rulechecker is keeping a cache, we ask it back"

	^ aRuleChecker manifestBuilderOfPackage: self item
]
