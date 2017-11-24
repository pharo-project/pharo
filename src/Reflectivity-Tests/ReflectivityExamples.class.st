"
Examples for RF
"
Class {
	#name : #ReflectivityExamples,
	#superclass : #Object,
	#instVars : [
		'ivar',
		'tag'
	],
	#classVars : [
		'ClassVar',
		'singleton'
	],
	#category : #'Reflectivity-Tests'
}

{ #category : #options }
ReflectivityExamples class >> exampleMethodWithMetaLinkOptionsViaClass [
	^ 2 + 3
]

{ #category : #options }
ReflectivityExamples class >> metaLinkOptions [
	^{
	#exampleMethodWithMetaLinkOptionsViaClass -> #( + optionCompileOnLinkInstallation)
	}
]

{ #category : #examples }
ReflectivityExamples >> exampleArray [
	^ {3}
]

{ #category : #examples }
ReflectivityExamples >> exampleAssignment [
	| temp |
	temp := (1 + 2).
	^temp
]

{ #category : #examples }
ReflectivityExamples >> exampleBlock [
	^ [2 + 3] value
]

{ #category : #examples }
ReflectivityExamples >> exampleBlockNoValue [
	^ [2 + 3]
]

{ #category : #examples }
ReflectivityExamples >> exampleClassVarRead [
	^ClassVar
]

{ #category : #examples }
ReflectivityExamples >> exampleGlobalRead [
	^GlobalForTesting
]

{ #category : #examples }
ReflectivityExamples >> exampleIfTrueIfFalse [
	| t |
	t := false.
	t ifTrue: [ self bar ] ifFalse: [ 'hello' ].
	^ 5 
]

{ #category : #examples }
ReflectivityExamples >> exampleIvarRead [
	^ivar 
]

{ #category : #examples }
ReflectivityExamples >> exampleLazyInit [
	^ singleton ifNil: [singleton := self new initialize]
]

{ #category : #examples }
ReflectivityExamples >> exampleLiteral [
	^ 2
]

{ #category : #examples }
ReflectivityExamples >> exampleLiteralArray [
	^ #(1)
]

{ #category : #examples }
ReflectivityExamples >> exampleMethod [
	^ 2 + 3
]

{ #category : #examples }
ReflectivityExamples >> exampleMethodMultipleSites [
	Transcript show: Object new.
	^ 2 + 3
]

{ #category : #examples }
ReflectivityExamples >> exampleMethodWithMetaLinkOptions [
	<metaLinkOptions: #( +optionCompileOnLinkInstallation)>
	^ 2 + 3
]

{ #category : #examples }
ReflectivityExamples >> examplePrimitiveMethod [
	"returns image path"
	<primitive: 121>
	^ 2 + 3
]

{ #category : #examples }
ReflectivityExamples >> exampleSendNoReturn [
	"just an example method that calls a mutation outside of a assignment or return node.
(needed for before link test with receiver reification"

	| collection |
	collection := OrderedCollection newFrom: {3 . 4}.
	collection removeAt:1.
]

{ #category : #examples }
ReflectivityExamples >> exampleSendTwoArgs [
	^ Array with: 1 with: 2.
]

{ #category : #examples }
ReflectivityExamples >> exampleTwoSends [
	3 + 4.
	^ 2 + 3
]

{ #category : #examples }
ReflectivityExamples >> exampleWithArg: anArg [
	^ 2 + anArg
]

{ #category : #initialization }
ReflectivityExamples >> initialize [
	ivar := 33.
	ClassVar := #AClassVar.
]

{ #category : #tagging }
ReflectivityExamples >> tag [
	^tag
]

{ #category : #tagging }
ReflectivityExamples >> tagExec: anObject [
	tag := anObject
]
