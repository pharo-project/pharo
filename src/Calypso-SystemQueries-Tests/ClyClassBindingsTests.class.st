Class {
	#name : #ClyClassBindingsTests,
	#superclass : #ClyQueryResultTestCase,
	#category : #'Calypso-SystemQueries-Tests-Results'
}

{ #category : #running }
ClyClassBindingsTests >> createQueryResult [
	^ClyClassBindings new
]

{ #category : #tests }
ClyClassBindingsTests >> testFillingWithMetaclass [

	queryResult fillWith: {Array class}.
	
	self assert: queryResult items equals: {Array binding}.
	
]

{ #category : #tests }
ClyClassBindingsTests >> testFillingWithTwoClasses [

	queryResult fillWith: {Array. String}.
	
	self assert: queryResult items equals: {Array binding. String binding}.
	
]
