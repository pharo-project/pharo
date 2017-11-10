Extension { #name : #Pragma }

{ #category : #'*GT-SpotterExtensions-Core' }
Pragma >> spotterActDefault [
    ^ self method browse
]

{ #category : #'*GT-SpotterExtensions-Core' }
Pragma >> spotterForBytecodesFor: aStep [
	<spotterOrder: 15>
	self method spotterForBytecodesFor: aStep
]

{ #category : #'*GT-SpotterExtensions-Core' }
Pragma >> spotterForImplementorsFor: aStep [
	<spotterOrder: 10>
	self method spotterForImplementorsFor: aStep
]

{ #category : #'*GT-SpotterExtensions-Core' }
Pragma >> spotterForSendersFor: aStep [
	<spotterOrder: 10>
	self method spotterForSendersFor: aStep
]

{ #category : #'*GT-SpotterExtensions-Core' }
Pragma >> spotterPreviewIn: composite [
	<spotterPreview: 10>
   self method spotterPreviewCodeIn: composite
]
