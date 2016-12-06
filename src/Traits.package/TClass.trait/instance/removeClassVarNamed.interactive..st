class variables
removeClassVarNamed: aString interactive: interactive
	"Remove the class variable whose name is the argument, aString, from 
    the names defined in the receiver, a class. Create an error notification if 
    aString is not a class variable or if it is still being used in the code of 
    the class."

	| aSymbol varUserClass |
	aSymbol := aString asSymbol.
	(self classPool includesKey: aSymbol)
		ifFalse: [ ^ self error: aString , ' is not a class variable' ].
	varUserClass := self anyUserOfClassVarNamed: aSymbol.
	varUserClass notNil & interactive ifTrue: [ 
		(self	confirm: (aString , ' is still used in code of class ' , varUserClass name , '.\Is it okay to move it to Undeclared?') withCRs)
			ifFalse: [ ^ self ] ].
	
	varUserClass 
		ifNotNil: [self crTrace: self name , ' (' , aString , ' is Undeclared) '.
			Undeclared declare: aSymbol from: self classPool]
		ifNil: [ self classPool removeKey: aSymbol].
		
	self classPool isEmpty
		ifTrue: [ self classPool: nil ].
		
	SystemAnnouncer uniqueInstance classModificationAppliedTo: self