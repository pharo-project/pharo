testing
allowsDeselection
	^ allowsDeselection ifNil: [ self class defaultAllowsDeselection ]