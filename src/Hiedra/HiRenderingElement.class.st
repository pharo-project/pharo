"
My hierarchy is used to render a Hiedra ruler.
"
Class {
	#name : #HiRenderingElement,
	#superclass : #Object,
	#category : 'Hiedra-Rendering'
}

{ #category : #controlling }
HiRenderingElement >> ifNode: nodeBlock ifLink: linkBlock [

	^ self subclassResponsibility
]

{ #category : #testing }
HiRenderingElement >> isLink [

	^ self subclassResponsibility
]
