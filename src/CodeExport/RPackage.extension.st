Extension { #name : #RPackage }

{ #category : #'*CodeExport' }
RPackage >> fileOut [

	| internalStream |
	internalStream := (String new: 1000) writeStream.

	self classTags do: [ :each | self organizer fileOutCategory: each categoryName on: internalStream ].

	extensionSelectors keysAndValuesDo: [ :class :selectors | selectors do: [ :selector | class fileOutMethod: selector on: internalStream ] ].

	^ CodeExporter writeSourceCodeFrom: internalStream baseName: self name isSt: true
]
