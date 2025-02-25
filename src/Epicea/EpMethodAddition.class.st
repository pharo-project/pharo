"
I represent a addition of a method in a class or a trait.
"
Class {
	#name : 'EpMethodAddition',
	#superclass : 'EpMethodChange',
	#instVars : [
		'method'
	],
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'instance creation' }
EpMethodAddition class >> method: aCompiledMethod [

	^ self basicNew
		initializeMethod: aCompiledMethod;
		yourself
]

{ #category : 'visitor' }
EpMethodAddition >> accept: aVisitor [

	^ aVisitor visitMethodAddition: self
]

{ #category : 'accessing' }
EpMethodAddition >> affectedPackageName [

	^ method package
]

{ #category : 'accessing' }
EpMethodAddition >> fileOutEntryOn: aWriteStream [
	"Write a code chunk to create the receiver's method on aWriteStream"

	self methodAffected origin 
		ifNotNil: [ : methodDefinition |
			self methodAffected origin 
				printMethodChunk: self methodAffected selector 
				on: aWriteStream ]
]

{ #category : 'initialization' }
EpMethodAddition >> initializeMethod: aCompiledMethod [

	self initialize.
	method := aCompiledMethod asEpiceaRingDefinition
]

{ #category : 'accessing' }
EpMethodAddition >> methodAdded [

	^ method
]

{ #category : 'accessing' }
EpMethodAddition >> methodAffected [

	^ self methodAdded
]

{ #category : 'accessing' }
EpMethodAddition >> protocol [

	^ method protocol
]

{ #category : 'accessing' }
EpMethodAddition >> selector [

	^ method selector
]

{ #category : 'accessing' }
EpMethodAddition >> sourceCode [

	^ method sourceCode
]
