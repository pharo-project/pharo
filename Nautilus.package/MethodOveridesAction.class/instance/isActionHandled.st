order
isActionHandled

	isOverridden := self isOverridden.
	isOverride := self isOverride.
	^ isOverridden or: [ isOverride ]