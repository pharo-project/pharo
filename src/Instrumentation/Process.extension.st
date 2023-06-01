Extension { #name : #Process }

{ #category : #'*Instrumentation' }
Process >> isActive: n [
	<metaLinkOptions: #( + optionDisabledLink)>
	^level = n or: [ level == nil ]
]

{ #category : #'*Instrumentation' }
Process >> isMeta [
	<metaLinkOptions: #( + optionDisabledLink)>
	level ifNil: [ level := 0 ].
	^level ~= 0
]

{ #category : #'*Instrumentation' }
Process >> level [
	<metaLinkOptions: #( + optionDisabledLink)>
	^level ifNil: [ level := 0 ]
]

{ #category : #'*Instrumentation' }
Process >> runInMetaLevel: aBlock [

	<metaLinkOptions: #( #+ optionDisabledLink )>
	self shiftLevelUp.
	^ aBlock insEnsure: InstrumentationDeactivator new
]

{ #category : #'*Instrumentation' }
Process >> shiftLevelDown [
	<metaLinkOptions: #( + optionDisabledLink)>
	level ifNil: [ level := 0 ].
	(level insIsEqual: 0) ifTrue: [ ^level ].
	(level insIsEqual: 1) ifTrue: [ ^level := 0 ].
	level := level insMinus: 1
]

{ #category : #'*Instrumentation' }
Process >> shiftLevelUp [
	<metaLinkOptions: #( + optionDisabledLink)>
	level ifNil: [ level := 0 ].
	level := level insPlus: 1
]
