Class {
	#name : #TabsExampleTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Deprecated80-Tests'
}

{ #category : #deprecation }
TabsExampleTest class >> isDeprecated [
	^ true
]

{ #category : #running }
TabsExampleTest >> classToTest [
	^ TabsExample
]
