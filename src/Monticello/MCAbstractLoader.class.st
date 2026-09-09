Class {
	#name : 'MCAbstractLoader',
	#superclass : 'Object',
	#instVars : [
		'isStrict'
	],
	#category : 'Monticello-Loading',
	#package : 'Monticello',
	#tag : 'Loading'
}

{ #category : 'initialization' }
MCAbstractLoader >> beStrict [

	"Force strict loading of definitions.
	A strict loading does not allow definitions of broken code"
	isStrict := true
]

{ #category : 'initialization' }
MCAbstractLoader >> initialize [

	super initialize.
	isStrict := false
]

{ #category : 'accessing' }
MCAbstractLoader >> isStrict [

	^ isStrict
]

{ #category : 'accessing' }
MCAbstractLoader >> isStrict: anObject [

	self assert: anObject notNil.
	isStrict := anObject
]
