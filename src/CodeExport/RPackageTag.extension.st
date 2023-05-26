Extension { #name : #RPackageTag }

{ #category : #'*CodeExport' }
RPackageTag >> fileOut [
	| internalStream |

	internalStream := (String new: 1000) writeStream.

	Smalltalk organization
		fileOutCategory: self categoryName
		on: internalStream.

	^ CodeExporter writeSourceCodeFrom: internalStream baseName: self name isSt: true
]
