Class {
	#name : #RBMethodProtocolChange,
	#superclass : #RBAddMethodChange,
	#category : #'Refactoring2-Core-Changes'
}

{ #category : #'instance creation' }
RBMethodProtocolChange class >> selector: aSelector in: aBehavior classified: aProtocol [

	^ self new
		selector: aSelector
		in: aBehavior
		classified: aProtocol
]

{ #category : #converting }
RBMethodProtocolChange >> asUndoOperation [
	
	^ self class
		selector: self selector
		in: self changeClass
		classified: self oldProtocol
]

{ #category : #printing }
RBMethodProtocolChange >> changeString [

	^ String streamContents: [ :stream | stream
		nextPutAll: '[';
		nextPutAll: self protocol;
		nextPutAll: '] ';
		nextPutAll: super changeString ]
]

{ #category : #utilities }
RBMethodProtocolChange >> oldProtocol [

	| oldProtocol |
	oldProtocol := RBBrowserEnvironment new
						whichProtocolIncludes: self selector in: self changeClass.
	^ oldProtocol ifNil: [ oldProtocol := #accessing ].
]

{ #category : #printing }
RBMethodProtocolChange >> oldVersionTextToDisplay [

	^ String streamContents: [ :stream | stream
		nextPutAll: '"protocol: ';
		nextPutAll: self oldProtocol;
		nextPutAll: '"';
		nextPutAll: String cr;
		nextPutAll: super oldVersionTextToDisplay ]
]

{ #category : #initialization }
RBMethodProtocolChange >> selector: aSelector in: aClass classified: aProtocol [

	selector := aSelector.
	self changeClass: aClass.
	self protocols: aProtocol
]

{ #category : #accessing }
RBMethodProtocolChange >> source [

	^ source ifNil: [ super oldVersionTextToDisplay ]
]

{ #category : #printing }
RBMethodProtocolChange >> textToDisplay [

	^ String streamContents: [ :stream | stream
		nextPutAll: '"protocol: ';
		nextPutAll: self protocol;
		nextPutAll: '"';
		nextPutAll: String cr;
		nextPutAll: super textToDisplay ]
]
