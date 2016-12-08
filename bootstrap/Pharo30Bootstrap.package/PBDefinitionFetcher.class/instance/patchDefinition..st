patching
patchDefinition: aDefinition

	| definition |
	
	definition := self environmentWithClassNameValidationDisbaledFrom: aDefinition.
	definition := self environmentWithTraitNameValidationDisbaledFrom: definition.
	definition := self environmentWithRestrictedVersionOfAddTraitSelectorWithMethodFrom: definition.
	"definition := self environmentWithoutStringTypeTableInitializationFrom: definition."
	definition := self environmentWithSlotValidationDisabled: definition.
	^ definition