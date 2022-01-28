Class {
	#name : #ClyMethodGroupTest,
	#superclass : #TestCase,
	#instVars : [
		'group'
	],
	#category : #'Calypso-SystemQueries-Tests-Domain'
}

{ #category : #running }
ClyMethodGroupTest >> createMethodGroup [
	self subclassResponsibility
]

{ #category : #running }
ClyMethodGroupTest >> setUp [
	super setUp.
	group := self createMethodGroup
]
