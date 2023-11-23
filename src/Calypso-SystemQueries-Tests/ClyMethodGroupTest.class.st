Class {
	#name : 'ClyMethodGroupTest',
	#superclass : 'TestCase',
	#instVars : [
		'group'
	],
	#category : 'Calypso-SystemQueries-Tests-Domain',
	#package : 'Calypso-SystemQueries-Tests',
	#tag : 'Domain'
}

{ #category : 'running' }
ClyMethodGroupTest >> createMethodGroup [
	self subclassResponsibility
]

{ #category : 'running' }
ClyMethodGroupTest >> setUp [
	super setUp.
	group := self createMethodGroup
]
