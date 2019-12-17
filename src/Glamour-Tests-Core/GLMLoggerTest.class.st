Class {
	#name : #GLMLoggerTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMLoggerTest >> testCustomInstance [
	| memoryLogger |
	self assert: GLMLogger instance identicalTo: GLMLogger nullInstance.
	memoryLogger := GLMMemoryLogger new.
	GLMLogger instance: memoryLogger.
	self assert: GLMLogger instance identicalTo: memoryLogger.
	GLMLogger reset.
	self assert: GLMLogger instance identicalTo: GLMLogger nullInstance
]

{ #category : #tests }
GLMLoggerTest >> testDefault [
	(GLMLoggedObject withAllSubclasses reject: [ :each | each allMethods anySatisfy: [ :cm | cm sendsSelector: #subclassResponsibility ] ])
		do: [ :each | self assert: each new logger identicalTo: GLMLogger nullInstance ]
]
