testing
hasAlreadyBeenInitializedBefore
	"Answers whether this shared pool was already initialized before or not"
	^ self platformName notNil and: [ self wordSize notNil ]
 