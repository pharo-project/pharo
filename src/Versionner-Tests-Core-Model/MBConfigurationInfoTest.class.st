"
SUnit tests for MBConfigurationInfo in Versionner
"
Class {
	#name : #MBConfigurationInfoTest,
	#superclass : #MBInfoTest,
	#instVars : [
		'info',
		'browser'
	],
	#category : #'Versionner-Tests-Core-Model'
}

{ #category : #running }
MBConfigurationInfoTest >> setUp [
	super setUp.

	info := MBConfigurationInfo configurationClass: configuration.

]

{ #category : #tests }
MBConfigurationInfoTest >> testBaselines [

	"+1 is for the default: baseline"
	self assert: info baselines size = ((configuration selectors select: [ :s | s beginsWith: 'baseline']) size + 1).

]

{ #category : #tests }
MBConfigurationInfoTest >> testDefaultVersion [
	
	self assert: (configuration selectors select: [ :s | s beginsWith: 'baseline']) size + 1 
		equals:	info baselines size

]
