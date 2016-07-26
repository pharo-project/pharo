logging
logSnapshot: save andQuit: quit
	"Log into the changes file the fact that the session will be either snapshoted, quit, or both."

	| message |	
	SourceFiles changesFileStream ifNil: [ ^ self ].

	message := String
		streamContents: [ :s | 
			s
				nextPutAll: '----';
				nextPutAll:
						(save
								ifTrue: [ 
									quit
										ifTrue: [ 'QUIT' ]
										ifFalse: [ 'SNAPSHOT' ] ]
								ifFalse: [ 
									quit
										ifTrue: [ 'QUIT/NOSAVE' ]
										ifFalse: [ 'NOP' ] ]);
				nextPutAll: '----';
				print: DateAndTime now;
				space;
				nextPutAll: (Smalltalk imageFile basename);
				nextPutAll: ' priorSource: ';
				print: Smalltalk lastQuitLogPosition ].

	self assureStartupStampLogged.

	save ifTrue: [ 
		Smalltalk lastQuitLogPosition:
			SourceFiles changesFileStream size ].

	self logChange: message