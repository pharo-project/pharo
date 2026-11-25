"
This rule warns when a Spec presenter (a subclass of SpPresenter) reads the Morphic theme with `Smalltalk ui theme` or `self theme`. Presenters should not reference the theme. Use the styles of Spec instead, defined and applied through the application, so that the presenter works with every backend.
"
Class {
	#name : 'ReSpecShouldNotReferenceMorphicThemeRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReSpecShouldNotReferenceMorphicThemeRule class >> groupSelector [

	^ #designFlawGroup
]

{ #category : 'accessing' }
ReSpecShouldNotReferenceMorphicThemeRule class >> ruleName [

	^ 'Spec presenters should not reference the Morphic theme'
]

{ #category : 'running' }
ReSpecShouldNotReferenceMorphicThemeRule >> basicCheck: aNode [

	| class |
	aNode isMessage ifFalse: [ ^ false ].
	aNode selector = #theme ifFalse: [ ^ false ].
	class := aNode methodNode methodClass.
	class ifNil: [ ^ false ].
	(class inheritsFrom: SpPresenter) ifFalse: [ ^ false ].
	^ (self isSmalltalkUiReference: aNode receiver) or: [ aNode receiver isVariable and: [ aNode receiver name = 'self' ] ]
]

{ #category : 'testing' }
ReSpecShouldNotReferenceMorphicThemeRule >> isSmalltalkUiReference: aNode [

	^ aNode isMessage and: [ aNode selector = #ui and: [ aNode receiver isVariable and: [ aNode receiver name = 'Smalltalk' ] ] ]
]
