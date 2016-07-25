instance creation
on: anEnvironment referencesToInstanceVariable: aString in: aClass 
	| newEnv |
	newEnv := (self onEnvironment: anEnvironment)
				label: 'References to ''' , aString , ''' in ' , aClass name;
				yourself.
	(aClass whichClassDefinesInstVar: aString) withAllSubclassesDo: 
			[:cls | 
			((cls whichSelectorsRead: aString) isEmpty not 
				or: [(cls whichSelectorsAssign: aString) isEmpty not]) 
					ifTrue: [newEnv addClass: cls instanceVariable: aString]].
	^newEnv