private
ensureInitalizeMethodExists: aClass
	(aClass includesSelector:  #initialize) ifTrue: [ ^self ].
	Author useAuthor: 'Generated' during: [
		aClass compile: 'initialize
	super initialize.' classified: 'initialization'.
		] 
	