accessing
source
	^ source ifNil: [ source := (class realClass sourceCodeAt: selector) asString ]