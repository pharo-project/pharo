Class {
	#name : #MalGraphStructure,
	#superclass : #MalGraphAlgorithm,
	#instVars : [
		'nodeClass',
		'edgeClass'
	],
	#category : #'Moose-Algos-Graph'
}

{ #category : #configuration }
MalGraphStructure >> edgeClass [

	^ edgeClass
]

{ #category : #configuration }
MalGraphStructure >> edgeClass: anObject [
	edgeClass := anObject
]

{ #category : #accessing }
MalGraphStructure >> edgeModels [

	^ self edges collect: [:e | e model]
]

{ #category : #configuration }
MalGraphStructure >> nodeClass [

	^ nodeClass
]

{ #category : #configuration }
MalGraphStructure >> nodeClass: anObject [
	nodeClass := anObject
]

{ #category : #accessing }
MalGraphStructure >> nodeModels [

	^ self nodes collect: [:n | n model]
]

{ #category : #running }
MalGraphStructure >> run [
	self shouldNotImplement
]
