Extension { #name : #Object }

{ #category : #'*Fuel-Core' }
Object >> fuelAccept: aGeneralMapper [

	"Be careful because the order is important. For example, weak are also variable, but we need that weak objects send #visitWeakObject: and not #visitVariableObject: "
	self class isFixed
		ifTrue: [ ^ aGeneralMapper visitFixedObject: self ].
	self class isWeak
		ifTrue: [ ^ aGeneralMapper visitWeakObject: self ].
	self class isPointers
		ifTrue: [ ^ aGeneralMapper visitVariableObject: self ].
	self class isBytes
		ifTrue: [ ^ aGeneralMapper visitBytesObject: self ].
	self class isWords
		ifTrue: [	^ aGeneralMapper visitWordsObject: self ].
	self error: 'Something is wrong!'
]

{ #category : #'*Fuel-Core' }
Object >> fuelAfterMaterialization [
	"Materialization process will send this message after materializing instances of my class"
]

{ #category : #'*Fuel-Core' }
Object >> fuelReplacement [
	^ self
]

{ #category : #'*Fuel-Core' }
Object >> serializeToFileNamed: aFilename [
	^ FLSerializer serialize: self toFileNamed: aFilename
]
