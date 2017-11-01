"
This class is a styler NullObject
"
Class {
	#name : #NullTextStyler,
	#superclass : #Object,
	#category : 'Morphic-Base-Text Support'
}

{ #category : #accessing }
NullTextStyler >> classOrMetaClass: aBehavior [
]

{ #category : #accessing }
NullTextStyler >> environment: anObject [

]

{ #category : #accessing }
NullTextStyler >> font: aFont [
]

{ #category : #formatting }
NullTextStyler >> format: aText [
	^aText
]

{ #category : #styling }
NullTextStyler >> style: aText [
]

{ #category : #styling }
NullTextStyler >> styleInBackgroundProcess: aText [

]

{ #category : #private }
NullTextStyler >> stylingEnabled [
	^ false
]

{ #category : #private }
NullTextStyler >> stylingEnabled: aBoolean [

]

{ #category : #styling }
NullTextStyler >> unstyledTextFrom: aText [
	^aText
]

{ #category : #accessing }
NullTextStyler >> view: aViewOrMorph [


]

{ #category : #accessing }
NullTextStyler >> workspace: aWorkspace [

]
