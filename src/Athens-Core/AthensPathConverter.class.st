"
i am a base class for path conversion. 
i take a path as input and producing a path commands as output.

My default implementation just passing all path commands without changes to destination without any conversion

Example of use:

 
converter := AthensXYZConverter dest: (AthensPolygonPath new).
convertedPath := converter convert: sourcePath.

You can chain multiple converters:

basicConverter := AthensZYXConv dest: AthensPolygonPath new. 
compoundConverter := AthensABCConverter dest: basicConverter.

compoundConverter convert: somePath

in this case, an initial conversion is performed by instance of AthensABCConverter, and then conversion results are passed down to AthensZYXConv,
and then finally to instance of AthensPolygonPath.

"
Class {
	#name : #AthensPathConverter,
	#superclass : #Object,
	#instVars : [
		'sourcePath',
		'dest',
		'endPoint'
	],
	#category : #'Athens-Core-Paths'
}

{ #category : #'instance creation' }
AthensPathConverter class >> dest: aDest [

	^ self new dest: aDest
]

{ #category : #'instance creation' }
AthensPathConverter class >> source: aSource dest: aDest [
	" answer a converted path from source to dest, using my instance as a converter"

	^ self new source: aSource dest: aDest
]

{ #category : #'converting path commands' }
AthensPathConverter >> close: aPoint [
	^ dest close: aPoint
]

{ #category : #actions }
AthensPathConverter >> convert: aSourcePath [

	" iterate over segments and do conversion "

	| segment |

	segment := aSourcePath.

	[ segment notNil ] whileTrue: [
		segment convertWith: self.
		endPoint := segment endPoint.
		segment := segment next.
	].

	^ dest finish
]

{ #category : #'converting path commands' }
AthensPathConverter >> curveVia: pt1 to: pt2 [

	^ dest curveVia: pt1 to: pt2
]

{ #category : #accessing }
AthensPathConverter >> dest: anObject [
	dest := anObject
]

{ #category : #actions }
AthensPathConverter >> finish [
	^ dest finish
]

{ #category : #'converting path commands' }
AthensPathConverter >> lineTo: aPoint [
	^ dest lineTo: aPoint
]

{ #category : #'converting path commands' }
AthensPathConverter >> moveTo: aPoint [
	^ dest moveTo: aPoint
]

{ #category : #accessing }
AthensPathConverter >> source: aSource dest: aDest [

	sourcePath := aSource.
	dest := aDest
]
