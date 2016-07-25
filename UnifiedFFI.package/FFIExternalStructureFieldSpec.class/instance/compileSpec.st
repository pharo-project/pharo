as yet unclassified
compileSpec 
	"This will compile the structure spec into something ExternalStructure can understand"
	^ fields values 
		inject: WordArray new
		into: [ :all :each | all, (each externalTypeWithArity embeddedSpecWithSize: each externalTypeSize) ]