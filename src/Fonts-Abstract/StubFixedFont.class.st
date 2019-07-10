"
I am font that provides no real glyphs nor displaying possibilities. I server only as a stub font that provides measurements on texts.
"
Class {
	#name : #StubFixedFont,
	#superclass : #AbstractFont,
	#instVars : [
		'baseFont'
	],
	#category : #'Fonts-Abstract'
}

{ #category : #accessing }
StubFixedFont >> ascent [
	^ 13
]

{ #category : #accessing }
StubFixedFont >> baseFont [
	^baseFont
]

{ #category : #accessing }
StubFixedFont >> baseFont: aFont [
	baseFont := aFont
]

{ #category : #accessing }
StubFixedFont >> baseKern [
	^ 0
]

{ #category : #accessing }
StubFixedFont >> descent [
	^ 4
]

{ #category : #accessing }
StubFixedFont >> descentKern [
	^ 0
]

{ #category : #displaying }
StubFixedFont >> displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta [ 
	
	"ignore"
]

{ #category : #displaying }
StubFixedFont >> displayString: aString on: aDisplayContext from: startIndex to: stopIndex at: aPoint kern: kernDelta baselineY: baselineY [

	"ignore"
]

{ #category : #accessing }
StubFixedFont >> emphasized: emph [
	
]

{ #category : #accessing }
StubFixedFont >> familyName [
	^ 'Stub Fixed Font'
]

{ #category : #accessing }
StubFixedFont >> familySizeFace [ 

	^ #('Stub Fixed Font' 10 0)
]

{ #category : #accessing }
StubFixedFont >> fontSize: aNumber [ 
	"ignore"
]

{ #category : #accessing }
StubFixedFont >> height [
	^17
]

{ #category : #displaying }
StubFixedFont >> installOn: aDisplayContext foregroundColor: foregroundColor backgroundColor: backgroundColor [
	"ignore"
]

{ #category : #accessing }
StubFixedFont >> lineGrid [
	^17
]

{ #category : #accessing }
StubFixedFont >> maxAscii [
	^ SmallInteger maxVal
]

{ #category : #accessing }
StubFixedFont >> pointSize [
	^10
]

{ #category : #measuring }
StubFixedFont >> widthOf: aCharacter [ 
	^ 5.4
]
