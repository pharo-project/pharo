"
After building a TestSuite and before executing its tests, we would like to apply to it some process like randomisation. I am an abstract representation of any class intented to do such things.
"
Class {
	#name : #AbstractSuitePreparator,
	#superclass : #Object,
	#category : #'TestRunner-Core-Preparators'
}

{ #category : #applying }
AbstractSuitePreparator class >> applyOn: testSuite [
	"this method defined the process which will applied in a giving suite."

	self subclassResponsibility
]

{ #category : #applying }
AbstractSuitePreparator class >> applyOnSuites: aCol [
	"apply to every testSuite the specified process (see self applyOn:)"

	aCol do: [ :suite | self applyOn: suite ]
]

{ #category : #applying }
AbstractSuitePreparator class >> isAbstract [

	^ self == AbstractSuitePreparator
]
