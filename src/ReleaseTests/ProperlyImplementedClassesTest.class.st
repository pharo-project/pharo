"
A unit test to verify that the classes are properly implemented
"
Class {
	#name : #ProperlyImplementedClassesTest,
	#superclass : #TestCase,
	#category : #ReleaseTests-Environment
}

{ #category : #tests }
ProperlyImplementedClassesTest >> testAllClassInstanceVariablesStartLowercase [
	"Make sure that all class instance variable names start with a lowercase letter"

	| violating |
	violating := OrderedCollection new.
	Object class withAllSubclassesDo: [ :each | 
			each instVarNames
				do: [ :var | 
					var asString first isUppercase
						ifTrue: [ violating add: each -> var ]]].
	self assert: violating isEmpty description: 'Class instance variable names must start with a lowercase letter'
]
