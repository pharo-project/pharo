"
I'm a base command for edition in code presenters. 
Operations like ""copy"", ""paste"" will be implemented by extending me. 
"
Class {
	#name : #SpTextEditionCommand,
	#superclass : #SpToolCommand,
	#category : #'Spec2-Commands-Code'
}

{ #category : #accessing }
SpTextEditionCommand class >> allOrdered [

	^ self allSubclasses sorted: #order ascending
]

{ #category : #accessing }
SpTextEditionCommand class >> order [
	"at the end by default"

	^ 999
]
