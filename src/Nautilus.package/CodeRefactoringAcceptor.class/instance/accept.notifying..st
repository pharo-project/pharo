protocol
accept: aText notifying: aController
	| namespace refactoring changes |
	self model sourceTextModel clearUserEdits.
	namespace := RBNamespace onEnvironment: self model browsedEnvironment.
	refactoring := self class compiler evaluate: aText.
	refactoring model: namespace.
	refactoring primitiveExecute.
	changes := refactoring changes changes.
	(ChangesBrowser changes: changes) open