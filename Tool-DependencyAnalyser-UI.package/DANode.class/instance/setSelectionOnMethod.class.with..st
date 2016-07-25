browse-nautilus
setSelectionOnMethod: aMethod class: aClass with: nautilus
	| protocol nautilusUI |
	aMethod ifNotNil: [ protocol := aMethod protocol ].
	nautilusUI := self rootNode packageTreeNautilusUI.
	self refreshNautilus: nautilus.
	nautilus
		browsedEnvironment: Nautilus defaultBrowsedEnvironment;
		showInstance: aClass isMeta not.
	nautilus
		package: aClass package
		class: aClass
		protocol: protocol
		method: aMethod.
	nautilusUI
		showHierarchy: false;
		giveFocusToMethod