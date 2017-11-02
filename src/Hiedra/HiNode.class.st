"
I am a node used to render a Hiedra ruler.
"
Class {
	#name : #HiNode,
	#superclass : #HiRenderingElement,
	#instVars : [
		'value',
		'columnIndex',
		'rowIndex'
	],
	#category : 'Hiedra-Rendering'
}

{ #category : #'instance creation' }
HiNode class >> contents: anObject [
	^ self new
		contents: anObject;
		yourself
]

{ #category : #'instance creation' }
HiNode class >> contents: anObject rowIndex: anIndex [
	^ self new
		contents: anObject;
		rowIndex: anIndex;
		yourself
]

{ #category : #comparing }
HiNode >> = other [

	^ self class = other class and: [ value = other contents ]
]

{ #category : #accessing }
HiNode >> columnIndex [
	^ columnIndex
]

{ #category : #accessing }
HiNode >> columnIndex: anObject [
	columnIndex := anObject
]

{ #category : #accessing }
HiNode >> contents [
	^ value
]

{ #category : #accessing }
HiNode >> contents: anObject [
	value := anObject
]

{ #category : #comparing }
HiNode >> hash [

	^ value hash
]

{ #category : #controlling }
HiNode >> ifNode: nodeBlock ifLink: linkBlock [

	^ nodeBlock cull: self
]

{ #category : #testing }
HiNode >> isLink [

	^ false
]

{ #category : #printing }
HiNode >> printOn: aStream [

	super printOn: aStream.
	
	aStream
		nextPut: $(;
		print: value;
		nextPut: $).
]

{ #category : #accessing }
HiNode >> rowIndex [
	^ rowIndex
]

{ #category : #accessing }
HiNode >> rowIndex: anObject [
	rowIndex := anObject
]

{ #category : #accessing }
HiNode >> rulerPoint [
	^ columnIndex @ rowIndex 
]
