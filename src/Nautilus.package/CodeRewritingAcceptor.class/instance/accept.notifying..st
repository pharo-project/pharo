protocol
accept: aText notifying: aController
	| environment tree rule result |
	self model sourceTextModel clearUserEdits.
	environment := self model browsedEnvironment.
	tree := self class compiler evaluate: aText.
	rule := RBTransformationRule new
		rewriteRule: tree;
		yourself.
	result := RBSmalllintChecker
		runRule: rule
		onEnvironment: environment.
	(ChangesBrowser changes: result builder changes) open