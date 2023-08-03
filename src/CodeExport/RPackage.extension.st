Extension { #name : #RPackage }

{ #category : #'*CodeExport' }
RPackage >> fileOut [

	| internalStream |
	internalStream := (String new: 1000) writeStream.

	self classTags do: [ :each | self organizer fileOutCategory: each categoryName on: internalStream ].

	classExtensionSelectors keysAndValuesDo: [ :class :selectors | selectors do: [ :selector | class fileOutMethod: selector on: internalStream ] ].

	metaclassExtensionSelectors keysAndValuesDo: [ :class :selectors | selectors do: [ :selector | class classSide fileOutMethod: selector on: internalStream ] ].

	^ CodeExporter writeSourceCodeFrom: internalStream baseName: self name isSt: true
]
