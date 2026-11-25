Extension { #name : 'ReExceptionProperty' }

{ #category : '*Renraku-UI' }
ReExceptionProperty >> actions [

	^ { (RePropertyAction new
		   icon: (self iconNamed: #smallDebug);
		   description: 'Debug the exception';
		   action: [ :prop |
			   (OupsDebugRequest newForContext: prop stack)
				   label: prop message;
				   submit ];
		   yourself) }
]
