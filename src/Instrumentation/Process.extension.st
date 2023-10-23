Extension { #name : 'Process' }

{ #category : '*Instrumentation' }
Process >> isActiveInMetaLevel: n [

	<metaLinkOptions: #( #+ optionDisabledLink )>
	<noInstrumentation>
	^ level = n or: [ level == nil ]
]

{ #category : '*Instrumentation' }
Process >> isMeta [

	<metaLinkOptions: #( #+ optionDisabledLink )>
	<noInstrumentation>
	level ifNil: [ level := 0 ].
	^ level ~= 0
]

{ #category : '*Instrumentation' }
Process >> level [

	<metaLinkOptions: #( #+ optionDisabledLink )>
	<noInstrumentation>
	^ level ifNil: [ level := 0 ]
]

{ #category : '*Instrumentation' }
Process >> runInMetaLevel: aBlock [

	<noInstrumentation>
	<metaLinkOptions: #( #+ optionDisabledLink )>
	self shiftLevelUp.
	^ aBlock insEnsure: InstrumentationDeactivator new
]

{ #category : '*Instrumentation' }
Process >> shiftLevelDown [

	<noInstrumentation>
	<metaLinkOptions: #( #+ optionDisabledLink )>
	level ifNil: [ level := 0 ].
	(level insIsEqual: 0) ifTrue: [ ^ level ].
	(level insIsEqual: 1) ifTrue: [ ^ level := 0 ].
	level := level insMinus: 1
]

{ #category : '*Instrumentation' }
Process >> shiftLevelUp [

	<metaLinkOptions: #( #+ optionDisabledLink )>
	<noInstrumentation>
	level ifNil: [ level := 0 ].
	level := level insPlus: 1
]
