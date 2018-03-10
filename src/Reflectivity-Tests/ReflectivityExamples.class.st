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
	<sampleInstance>
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
	<sampleInstance>
	^ {3}
]

{ #category : #examples }
ReflectivityExamples >> exampleAssignment [
	<sampleInstance>
	| temp |
	temp := (1 + 2).
	^temp
]

{ #category : #examples }
ReflectivityExamples >> exampleBlock [
	<sampleInstance>
	^ [2 + 3] value
]

{ #category : #examples }
ReflectivityExamples >> exampleBlockNoValue [
	<sampleInstance>
	^ [2 + 3]
]

{ #category : #examples }
ReflectivityExamples >> exampleClassVarRead [
	<sampleInstance>
	^ClassVar
]

{ #category : #examples }
ReflectivityExamples >> exampleGlobalRead [
	<sampleInstance>
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
	<sampleInstance>
	^ivar 
]

{ #category : #examples }
ReflectivityExamples >> exampleLazyInit [
	<sampleInstance>
	^ singleton ifNil: [singleton := self new initialize]
]

{ #category : #examples }
ReflectivityExamples >> exampleLiteral [
	<sampleInstance>
	^ 2
]

{ #category : #examples }
ReflectivityExamples >> exampleLiteralArray [
	<sampleInstance>
	^ #(1)
]

{ #category : #examples }
ReflectivityExamples >> exampleMethod [

	^ 2 + 3
]

{ #category : #examples }
ReflectivityExamples >> exampleMethodMultipleSites [
	<sampleInstance>
	Transcript show: Object new.
	^ 2 + 3
]

{ #category : #examples }
ReflectivityExamples >> exampleMethodWithMetaLinkOptions [
	<sampleInstance>
	<metaLinkOptions: #( +optionCompileOnLinkInstallation)>
	^ 2 + 3
]

{ #category : #examples }
ReflectivityExamples >> examplePrimitiveMethod [
	<sampleInstance>
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
	<sampleInstance>
	^ Array with: 1 with: 2
]

{ #category : #examples }
ReflectivityExamples >> exampleTwoSends [
	<sampleInstance>
	3 + 4.
	^ 2 + 3
]

{ #category : #examples }
ReflectivityExamples >> exampleWithArg: anArg [
	<sampleInstance>
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
