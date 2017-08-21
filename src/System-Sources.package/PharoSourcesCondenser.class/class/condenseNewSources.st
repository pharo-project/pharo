helper
condenseNewSources
	| fileName condenser arch |

	arch := (Smalltalk vm wordSize = 4) ifTrue: [32] ifFalse: [64].
	
	fileName := 'Pharo{1}.{2}-{3}bit-{4}' format: {SystemVersion current major asString. SystemVersion current minor asString. arch. SystemVersion current commitHashShort asString}.
	
	condenser := self new.
	condenser newSourceNamed: fileName.
	condenser condense.