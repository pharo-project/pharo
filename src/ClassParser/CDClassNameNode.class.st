"
I represent the name of classs, i may be removed in the future if i don't serve another purpose.
"
Class {
	#name : #CDClassNameNode,
	#superclass : #CDNode,
	#instVars : [
		'className'
	],
	#category : #'ClassParser-Model'
}

{ #category : #accessing }
CDClassNameNode >> className [
	
	^ className
]

{ #category : #accessing }
CDClassNameNode >> className: aString [ 
	
	className := aString
]

{ #category : #accessing }
CDClassNameNode >> name [
	
	^ className
]

{ #category : #accessing }
CDClassNameNode >> value [
	
	^ className
]
