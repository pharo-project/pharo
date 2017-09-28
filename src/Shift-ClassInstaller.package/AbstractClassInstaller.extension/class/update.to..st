*Shift-ClassInstaller
update: aClass to: aBlock
	| builder |
	builder := self new builder.
	aBlock value: builder.
	^ builder build