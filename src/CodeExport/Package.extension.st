Extension { #name : 'Package' }

{ #category : '*CodeExport' }
Package >> fileOut [

	| internalStream |
	internalStream := (String new: 1000) writeStream.

	self tags do: [ :tag | tag fileOutOn: internalStream ].
	extensionSelectors keysAndValuesDo: [ :class :selectors | selectors do: [ :selector | class fileOutMethod: selector on: internalStream ] ].

	^ CodeExporter writeSourceCodeFrom: internalStream baseName: self name isSt: true
]
