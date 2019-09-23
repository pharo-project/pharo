Class {
	#name : #ReflectivityExamples2Subclass,
	#superclass : #ReflectivityExamples2,
	#category : #'Reflectivity-Tests'
}

{ #category : #example }
ReflectivityExamples2Subclass >> methodWithOverrides [
	^super methodWithOverrides, ' + down'
]

{ #category : #example }
ReflectivityExamples2Subclass >> methodWithOverrides: str1 with: str2 [
	^(super methodWithOverrides: str1 with: str2), str1, str2
]
