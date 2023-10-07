Class {
	#name : 'EqPersonComparisonTest',
	#superclass : 'TestCase',
	#category : 'Equals-Tests',
	#package : 'Equals-Tests'
}

{ #category : 'tests' }
EqPersonComparisonTest >> testComparisonOfStudentAndTeacherOfSameAgeShouldNotBeEqual [
	| luc tom |
	luc := EqTeacher name: 'Luc' age: 24.
	tom := EqStudent name: 'Tom' age: 24.
	
	self deny: luc equals: tom
]

{ #category : 'tests' }
EqPersonComparisonTest >> testComparisonOfStudentAndTeacherOfSameNameAndAgeShouldNotBeEquals [
	| smith1 smith2 |
	"Not equal beause instances of different classes"
	smith1 := EqTeacher name: 'Smith' age: 24.
	smith2 := EqStudent name: 'Smith' age: 24.
	
	self deny: smith1 equals: smith2
]

{ #category : 'tests' }
EqPersonComparisonTest >> testComparisonOfStudentOfDifferentAgeShouldNotBeEqual [
	| luc tom |
	
	luc := EqStudent name: 'Luc' age: 24.
	tom := EqStudent name: 'Tom' age: 5.
	
	self deny: luc equals: tom
]

{ #category : 'tests' }
EqPersonComparisonTest >> testComparisonOfStudentOfDifferentLevelShouldNotBeEqual [
	| luc tom |
	
	luc := EqStudent name: 'Luc' age: 24.
	luc level: 7.
	tom := EqStudent name: 'Tom' age: 24.
	tom level: 4.
		
	self deny: luc equals: tom
]

{ #category : 'tests' }
EqPersonComparisonTest >> testComparisonOfStudentOfSameAgeAndLevelShouldBeEqual [
	| luc tom |
	
	luc := EqStudent name: 'Luc' age: 24.
	luc level: 7.
	tom := EqStudent name: 'Tom' age: 24.
	tom  level: 7.
		
	self assert: tom equals: luc
]
