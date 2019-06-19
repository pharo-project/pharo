Class {
	#name : #ClyMethodGroupTestCase,
	#superclass : #TestCase,
	#instVars : [
		'group'
	],
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyMethodGroupTestCase >> createMethodGroup [
	self subclassResponsibility 
]

{ #category : #running }
ClyMethodGroupTestCase >> setUp [
	super setUp.
	group := self createMethodGroup
]
