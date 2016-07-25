running
resetResources: topLevelResources
	"Reset all imperfectly-set-up resources while gathering the rest for ordered resetting."

	| availableResources |
	availableResources := OrderedCollection new: topLevelResources size.
	topLevelResources
		do: [:each | each resetOrAddResourcesTo: availableResources].
	availableResources reverseDo: [:each | each reset]