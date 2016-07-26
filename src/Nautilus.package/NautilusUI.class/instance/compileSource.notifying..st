source code area
compileSource: aText notifying: aController
	aText ifEmpty: [ ^ self ].
	acceptor accept: aText asString notifying: aController.