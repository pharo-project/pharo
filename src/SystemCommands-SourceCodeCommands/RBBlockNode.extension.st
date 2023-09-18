Extension { #name : #RBBlockNode }

{ #category : #'*SystemCommands-SourceCodeCommands' }
RBBlockNode >> displaySourceCode [

	| code |
	code := '[ '.
	self arguments ifNotEmpty: [ :args |
		code := code , (String streamContents: [ :s |
			         args do: [ :arg |
				         s
					         nextPutAll: ':';
					         nextPutAll: arg name asString;
					         nextPutAll: ' ' ] ]).
		code := code , '| ' ].
	code := code , '  ]'.
	^ code
]
