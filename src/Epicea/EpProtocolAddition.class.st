"
I represent the addition of a protocol of a class or a trait.
"
Class {
	#name : 'EpProtocolAddition',
	#superclass : 'EpProtocolChange',
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'visitor' }
EpProtocolAddition >> accept: aVisitor [
	^ aVisitor visitProtocolAddition: self
]

{ #category : 'accessing' }
EpProtocolAddition >> fileOutEntryOn: aWriteStream [ 

	self error: 'Cannot file out protocol additions. Importing a single protocol addition will reset the entire organization.'
]
