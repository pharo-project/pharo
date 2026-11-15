"
I stand for a browser tool in the tests of the tabs of command maps. I refuse the contexts of ClyMockToolSubContext.
"
Class {
	#name : 'ClyMockTabTool',
	#superclass : 'Object',
	#category : 'Calypso-Commands-Tests',
	#package : 'Calypso-Commands-Tests'
}

{ #category : 'instance creation' }
ClyMockTabTool class >> for: aBrowser inContext: aToolContext [

	^ self new
]

{ #category : 'testing' }
ClyMockTabTool class >> shouldBeActivatedInContext: aToolContext [

	^ (aToolContext isKindOf: ClyMockToolSubContext) not
]
