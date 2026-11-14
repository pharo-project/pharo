"
I stand for a table decorator in the tests of the table decorators of command maps. My priority is 5 and I refuse the contexts of ClyMockToolSubContext.
"
Class {
	#name : 'ClyMockTableDecorator',
	#superclass : 'Object',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'accessing' }
ClyMockTableDecorator class >> priority [

	^ 5
]

{ #category : 'testing' }
ClyMockTableDecorator class >> wantsDecorateTableCellInContext: aToolContext [

	^ (aToolContext isKindOf: ClyMockToolSubContext) not
]
