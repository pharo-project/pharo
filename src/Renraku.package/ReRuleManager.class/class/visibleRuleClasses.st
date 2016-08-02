utility
visibleRuleClasses

	^ (ReAbstractRule withAllSubclasses select: #isVisible)
	"remove this in the future ->", self visibleLintRuleClasses