"
My API has methods whose implementation might depend on the Pharo image version.
"
Class {
	#name : #EpPlatform,
	#superclass : #Object,
	#classInstVars : [
		'current'
	],
	#category : #'Epicea-Support'
}

{ #category : #accessing }
EpPlatform class >> current [

	^ current ifNil: [ current := self new ]
]

{ #category : #convenience }
EpPlatform >> packageNameFor: aClassOrTrait [

	^ (self packageOrganizer packageOf: aClassOrTrait)
		  ifNotNil: [ :package | package name ]
		  ifNil: [ 'nil' ]
]
