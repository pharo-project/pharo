instance creation
open

	| env rules  |
	rules :=  RBCompositeLintRule allGoodRules rules.
	env := RBBrowserEnvironment default.
	self openOnRule: rules onEnvironment:  env