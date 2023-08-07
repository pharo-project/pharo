Extension { #name : #RPackageTag }

{ #category : #'*CodeExport' }
RPackageTag >> fileOut [
	| internalStream |

	internalStream := (String new: 1000) writeStream.

	self packageOrganizer
		fileOutCategory: self categoryName
		on: internalStream.

	^ CodeExporter writeSourceCodeFrom: internalStream baseName: self name isSt: true
]
