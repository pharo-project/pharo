accessing
customProjectAttributes
	Smalltalk os isMacOS
		ifTrue: [ (Smalltalk at: #PipeableOSProcess ifAbsent: [  ])
				ifNil: [ (Smalltalk at: #OSSUnixSubprocess ifAbsent: [  ])
						ifNil: [ ^ #(#MacOSSubprocess) ]
						ifNotNil: [ ^ #(#MacOS) ] ]
				ifNotNil: [ ^ #(#MacOS) ] ].
	Smalltalk os isUnix
		ifTrue: [ ^ (Smalltalk at: #PipeableOSProcess ifAbsent: [  ])
				ifNil: [ (Smalltalk at: #OSSUnixSubprocess ifAbsent: [  ])
						ifNil: [ ^ #(#UnixOSSubprocess) ]
						ifNotNil: [ ^ #(#Unix) ] ]
				ifNotNil: [ ^ #(#Unix) ] ].
	Smalltalk os isWindows
		ifTrue: [ ^ #(#Windows) ]