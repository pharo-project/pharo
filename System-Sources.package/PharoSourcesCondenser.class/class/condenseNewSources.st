helper
condenseNewSources
	| condenser |
	condenser := self new.
	condenser newSourceVersion: (SystemVersion current major) * 10.
	condenser condense.