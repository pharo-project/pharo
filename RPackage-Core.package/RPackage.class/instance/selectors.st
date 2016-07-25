accessing
selectors

	| sel |
	sel := Set new.
	metaclassExtensionSelectors valuesDo: [:each | sel addAll: each].
	classExtensionSelectors valuesDo: [:each | sel addAll: each].
	metaclassDefinedSelectors valuesDo: [:each | sel addAll: each].
	classDefinedSelectors valuesDo: [:each | sel addAll: each].
	^ sel
	