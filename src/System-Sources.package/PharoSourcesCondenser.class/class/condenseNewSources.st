helper
condenseNewSources
	| fileName condenser |
	
	fileName := 'Pharo{1}.{2}-{3}' format: {SystemVersion current major asString. SystemVersion current minor asString. SystemVersion current commitHashShort asString}.
	
	condenser := self new.
	condenser newSourceNamed: fileName.
	condenser condense.