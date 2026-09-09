Class {
	#name : 'AbstractReporterTest',
	#superclass : 'TestCase',
	#instVars : [
		'reporter'
	],
	#category : 'TestRunner-Tests-Reporters',
	#package : 'TestRunner-Tests',
	#tag : 'Reporters'
}

{ #category : 'testing' }
AbstractReporterTest class >> isAbstract [

	^ self == AbstractReporterTest
]

{ #category : 'accessing' }
AbstractReporterTest >> reporter [

	^ reporter
]
