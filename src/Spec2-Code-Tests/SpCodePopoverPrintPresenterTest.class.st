Class {
	#name : #SpCodePopoverPrintPresenterTest,
	#superclass : #TestCase,
	#instVars : [
		'presenter'
	],
	#category : #'Spec2-Code-Tests'
}

{ #category : #running }
SpCodePopoverPrintPresenterTest >> setUp [

	super setUp.
	presenter := SpCodePopoverPrintPresenter new
]

{ #category : #tests }
SpCodePopoverPrintPresenterTest >> testObjectAsString [
	| string |

	presenter setModelBeforeInitialization: 10 factorial. 
	self 
		assert: presenter objectAsString 
		equals: '3628800'.

	presenter setModelBeforeInitialization: 42 factorial. 
	self 
		assert: presenter objectAsString 
		equals: '1405006117752879898543142606244511569936384000000000'.

	presenter setModelBeforeInitialization: 150 factorial.
	self 
		assert: presenter objectAsString 
		equals: (String streamContents: [ :stream | 
			(150 factorial asString 
				groupsOf: presenter class maxLengthShownInPopover)
				do: [ :each | stream << each ]
				separatedBy: [ stream cr ] ]).	
		
	string := String loremIpsum withNoLineLongerThan: presenter class maxLengthShownInPopover.
	presenter setModelBeforeInitialization: string.
	self 
		assert: presenter objectAsString 
		equals: string printString.
		
	string := String loremIpsum withNoLineLongerThan: 10.
	presenter setModelBeforeInitialization: string.
	self 
		assert: presenter objectAsString 
		equals: string printString
]
