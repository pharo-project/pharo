*Manifest-Core
manifestBuilderForRuleChecker: aRuleChecker
	"Return the manifestsince the rulechecker is keeping a cache, we ask it back"

	^ aRuleChecker manifestBuilderOfClass: self