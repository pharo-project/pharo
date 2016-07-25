top window
adjustBorderUponDeactivationWhenLabeless
	"Adjust the border upon deactivation when, labelless"

	| aWidth |
	(aWidth := self valueOfProperty: #borderWidthWhenInactive) ifNotNil:
		[self acquireBorderWidth: aWidth]