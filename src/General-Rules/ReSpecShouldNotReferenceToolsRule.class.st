"
This rule warn the user if `Smalltalk tools` is referenced in a Spec subclass. Presenters should access the tools via the application doing `self application tools` instead.
"
Class {
	#name : 'ReSpecShouldNotReferenceToolsRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
ReSpecShouldNotReferenceToolsRule class >> ruleName [

	^ 'Spec subclasses should not reference Smalltalk tools'
]

{ #category : 'running' }
ReSpecShouldNotReferenceToolsRule >> basicCheck: aNode [

	| class |
	aNode isMessage ifFalse: [ ^ false ].
	class := aNode methodNode methodClass.
	class ifNil: [ ^ false ].
	(class inheritsFrom: StPresenter) ifFalse: [ ^ false ].
	^ self isToolReference: aNode
]

{ #category : 'testing' }
ReSpecShouldNotReferenceToolsRule >> isToolReference: aMessageNode [

	aMessageNode receiver isVariable ifTrue: [
			Smalltalk tools tools valuesDo: [ :toolClass |
				toolClass name = aMessageNode receiver name ifTrue: [ ^ true ] ] ].

	(aMessageNode receiver isVariable and: [
		 aMessageNode receiver name = 'Smalltalk' ]) ifFalse: [ ^ false ].

	^ aMessageNode keywords = #( 'tools' )
]
