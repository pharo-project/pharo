Class {
	#name : #ClassWithBadAnnotatingMethod,
	#superclass : #Object,
	#category : #'ClassAnnotation-Tests-Examples-Annotated classes'
}

{ #category : #annotations }
ClassWithBadAnnotatingMethod class >> badAnnotation [
	<classAnnotation>
	
	"bad annotation do not return an annotation instance.
	In that case user forgot to return anything. And the result will be class itself"
]
