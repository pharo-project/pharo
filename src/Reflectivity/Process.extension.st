Extension { #name : #Process }

{ #category : #'*Reflectivity' }
Process >> isActive: n [
	<metaLinkOptions: #( + optionDisabledLink)>
	^level = n or: [ level == nil ]. 
]

{ #category : #'*Reflectivity' }
Process >> isMeta [
	<metaLinkOptions: #( + optionDisabledLink)>
	level ifNil: [ level := 0 ].
	^level ~= 0
]

{ #category : #'*Reflectivity' }
Process >> level [
	<metaLinkOptions: #( + optionDisabledLink)>
	^level ifNil: [ level := 0 ]
]

{ #category : #'*Reflectivity' }
Process >> rfeffectiveProcess [
	"same as #effectiveProcess but for internal use for metalink activation"
	<metaLinkOptions: #( + optionDisabledLink)>
	^effectiveProcess ifNil: [self] 
]

{ #category : #'*Reflectivity' }
Process >> shiftLevelDown [
	<metaLinkOptions: #( + optionDisabledLink)>
	level ifNil: [ level := 0 ].
	(level rfIsEqual: 0) ifTrue: [ ^level ].
	(level rfIsEqual: 1) ifTrue: [ ^level := 0 ].
	level := level rfMinus: 1.
]

{ #category : #'*Reflectivity' }
Process >> shiftLevelUp [
	<metaLinkOptions: #( + optionDisabledLink)>
	level ifNil: [ level := 0 ].
	level := level rfPlus: 1
]
