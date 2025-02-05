Extension { #name : 'Object' }

{ #category : '*Fuel-Core' }
Object >> fuelAccept: aGeneralMapper [
	^ self class classLayout
		fuelAccept: aGeneralMapper
		forInstance: self
]

{ #category : '*Fuel-Core' }
Object >> fuelAfterMaterialization [
	"Materialization process will send this message after materializing instances of my class"
]

{ #category : '*Fuel-Core' }
Object >> fuelCheckEphemeronSupport [
	FLNotSerializable signal: 'Unsupported ephemeron implementation'
]

{ #category : '*Fuel-Core' }
Object >> fuelWasMourned [
	^ false
]

{ #category : '*Fuel-Core' }
Object >> serializeOn: aStream [
	FLSerializer
		serialize: self
		on: aStream
]

{ #category : '*Fuel-Core' }
Object >> serializeToByteArray [
	^ FLSerializer serializeToByteArray: self
]

{ #category : '*Fuel-Core' }
Object >> serializeToFileNamed: aFilename [
	FLSerializer
		serialize: self
		toFileNamed: aFilename
]
