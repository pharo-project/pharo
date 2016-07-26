commands
add: aSpec wrapped: aBlock
	| wrapper |
	wrapper := SpecWrapperTableLayout wrap: aSpec.
	aBlock value: wrapper.
	self add: wrapper
	