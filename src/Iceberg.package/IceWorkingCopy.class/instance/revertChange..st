actions
revertChange: anIceMethodChange
	| loader |
	loader := MCPackageLoader new.
	anIceMethodChange operation inverse applyTo: loader.
	loader load.
