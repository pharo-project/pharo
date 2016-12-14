actions
exportToStFiles: aDirectoryName initializers: initializers 
	
	| aDirectory |
	aDirectory := aDirectoryName asFileReference ensureCreateDirectory.	
	self withMczDo: [ :mcz |
		(aDirectory / (mcz fileName asFileReference basenameWithoutExtension , (initializers ifFalse: [ '.no_init' ] ifTrue: [''])) , 'st') 
			writeStreamDo: [ :stream | | writer |
				writer := MCStWriter on: stream.
				writer writeInitializers: initializers.
				writer writeDefinitions: mcz snapshot definitions ]]