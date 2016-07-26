instance creation
on: anEnvironment writersOfInstanceVariable: aString in: aClass 
	| newEnv |
	newEnv := (self onEnvironment: anEnvironment)
				label: 'Writers of ''' , aString , ''' in ' , aClass name;
				yourself.
	(aClass whichClassDefinesInstVar: aString) withAllSubclassesDo: 
			[:cls | 
			(cls whichSelectorsAssign: aString) isEmpty
				ifFalse: [newEnv addClass: cls instanceVariableWriter: aString]].
	^newEnv