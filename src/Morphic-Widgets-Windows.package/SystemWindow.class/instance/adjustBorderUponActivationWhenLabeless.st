top window
adjustBorderUponActivationWhenLabeless
	"Adjust the border upon, um, activation when, um, labelless"

	| aWidth |
	(aWidth := self valueOfProperty: #borderWidthWhenActive) ifNotNil:
		[self acquireBorderWidth: aWidth]