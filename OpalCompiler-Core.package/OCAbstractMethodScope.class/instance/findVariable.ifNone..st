lookup
findVariable: aBlock ifNone: aNotFound

	(aBlock value: thisContextVar) ifTrue: [^ thisContextVar].
	self allTemps
		detect: [ :var | aBlock value: var ]
		ifFound: [ :var | ^ var ].
	
	outerScope ifNotNil: [
		^  outerScope findVariable: aBlock ifNone: aNotFound ]