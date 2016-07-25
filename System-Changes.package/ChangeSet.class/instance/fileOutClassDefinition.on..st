private
fileOutClassDefinition: class on: stream
	"Write out class definition for the given class on the given stream, if the class definition was added or changed."

	(self atClass: class includes: #rename)
		ifTrue: [ 
			stream
				nextChunkPut: 'Smalltalk renameClassNamed: #' , (self oldNameFor: class) , ' as: #' , class name;
				cr ].
	(self atClass: class includes: #change)
		ifTrue: [ 
			"fat definition only needed for changes"
			stream
				nextChunkPut: (self fatDefForClass: class);
				cr.
			DeepCopier new checkClass: class	"If veryDeepCopy weakly copies some inst 
			vars in this class, warn author when new ones are added." ]
		ifFalse: [ 
			(self atClass: class includes: #add)
				ifTrue: [ 
					"use current definition for add"
					stream
						nextChunkPut: class definition;
						cr.
					DeepCopier new checkClass: class	"If veryDeepCopy weakly copies some inst 
				vars in this class, warn author when new ones are added." ] ].
	(self atClass: class includes: #comment)
		ifFalse: [ ^ self ].
	class theNonMetaClass organization
		putCommentOnFile: stream
		forClass: class theNonMetaClass.
	stream cr