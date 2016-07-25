accessing
extensionSelectors
	"Extension methods are methods defined on classes that are not defined in the receiver"

	| classSels |
	classSels := Set new.
	classExtensionSelectors valuesDo: [ :each | classSels addAll: each ].
	metaclassExtensionSelectors valuesDo: [ :each | classSels addAll: each ].
	^ classSels