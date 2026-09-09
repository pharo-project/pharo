Class {
	#name : 'ReMethodProtocolChange',
	#superclass : 'ReAddMethodChange',
	#category : 'Refactoring-Changes',
	#package : 'Refactoring-Changes'
}

{ #category : 'instance creation' }
ReMethodProtocolChange class >> selector: aSelector in: aBehavior classified: aProtocol [

	^ self new
		selector: aSelector
		in: aBehavior
		classified: aProtocol
]

{ #category : 'converting' }
ReMethodProtocolChange >> asUndoOperation [

	^ self class
		selector: self selector
		in: self changeClass
		classified: self oldProtocol
]

{ #category : 'printing' }
ReMethodProtocolChange >> changeString [

	^ String streamContents: [ :stream | stream
		nextPutAll: '[';
		nextPutAll: self protocol;
		nextPutAll: '] ';
		nextPutAll: super changeString ]
]

{ #category : 'utilities' }
ReMethodProtocolChange >> oldProtocol [

	| oldProtocol |
	oldProtocol := RBBrowserEnvironment new
						whichProtocolIncludes: self selector in: self changeClass.
	^ oldProtocol ifNil: [ oldProtocol := #accessing ]
]

{ #category : 'printing' }
ReMethodProtocolChange >> oldVersionTextToDisplay [

	^ String streamContents: [ :stream | stream
		nextPutAll: '"protocol: ';
		nextPutAll: self oldProtocol;
		nextPutAll: '"';
		nextPutAll: String cr;
		nextPutAll: super oldVersionTextToDisplay ]
]

{ #category : 'initialization' }
ReMethodProtocolChange >> selector: aSelector in: aClass classified: aProtocol [

	selector := aSelector.
	self changeClass: aClass.
	self protocols: aProtocol
]

{ #category : 'accessing' }
ReMethodProtocolChange >> source [

	^ source ifNil: [ super oldVersionTextToDisplay ]
]

{ #category : 'printing' }
ReMethodProtocolChange >> textToDisplay [

	^ String streamContents: [ :stream | stream
		nextPutAll: '"protocol: ';
		nextPutAll: self protocol;
		nextPutAll: '"';
		nextPutAll: String cr;
		nextPutAll: super textToDisplay ]
]
