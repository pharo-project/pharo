Class {
	#name : 'ReGeneratePrintOnDriverTest',
	#superclass : 'ReDriverTest',
	#instVars : [
		'testingEnvironment'
	],
	#category : 'Refactoring-UI-Tests',
	#package : 'Refactoring-UI-Tests'
}

{ #category : 'private' }
ReGeneratePrintOnDriverTest >> classForPrintOn [
	"Answer the <Class> which will be used to generate printOn: method"

	^ ReClassForGeneratingPrintOn 
]

{ #category : 'tests' }
ReGeneratePrintOnDriverTest >> testGeneratePrintOn [

	| driver rbClass driverChanges |

	testingEnvironment := RBClassEnvironment class: self classForPrintOn. 	
	driver := ReGeneratePrintOnDriver basicNew. 
	self setUpDriver: driver.
	driver scopes: { testingEnvironment }.
	rbClass := testingEnvironment class.

	self deny: (driver targetClass definesMethod: #printOn:).

	driver selectDialog selectedItems: #(#iVarA #iVarB).
	driver runRefactoring.
	driverChanges := driver refactoring changes.

	self 
		assert: driverChanges changes size 
		equals: 1.
	self assertEmpty: driver refactoring failedApplicabilityPreconditions.
	self 
		assert: (driverChanges changes select: [ : amc | amc selector = #printOn: ]) size
		equals: 1.
]
