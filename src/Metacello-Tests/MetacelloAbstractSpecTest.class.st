Class {
	#name : 'MetacelloAbstractSpecTest',
	#superclass : 'TestCase',
	#category : 'Metacello-Tests-Specs',
	#package : 'Metacello-Tests',
	#tag : 'Specs'
}

{ #category : 'baselines' }
MetacelloAbstractSpecTest >> baseline: spec [

	<baseline>
	
]

{ #category : 'specs' }
MetacelloAbstractSpecTest >> groupSpec [

	^ self project groupSpec
]

{ #category : 'accessing' }
MetacelloAbstractSpecTest >> packageSpec [

	^ self project packageSpec
]

{ #category : 'accessing' }
MetacelloAbstractSpecTest >> project [

	^ (MetacelloBaselineConstructor on: self) project
]

{ #category : 'specs' }
MetacelloAbstractSpecTest >> projectSpec [

	^ MetacelloProjectSpec project: self project
]
