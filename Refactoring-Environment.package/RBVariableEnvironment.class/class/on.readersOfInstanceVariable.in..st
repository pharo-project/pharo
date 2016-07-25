instance creation
on: anEnvironment readersOfInstanceVariable: aString in: aClass 
	| newEnv |
	newEnv := (self onEnvironment: anEnvironment)
				label: 'Readers of ''' , aString , ''' in ' , aClass name;
				yourself.
	(aClass whichClassDefinesInstVar: aString) withAllSubclassesDo: 
			[:cls | 
			(cls whichSelectorsRead: aString) isEmpty
				ifFalse: [newEnv addClass: cls instanceVariableReader: aString]].
	^newEnv