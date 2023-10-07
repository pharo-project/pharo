"
I represent a package creation in the system.
"
Class {
	#name : 'EpPackageAddition',
	#superclass : 'EpPackageChange',
	#category : 'Epicea-Model',
	#package : 'Epicea',
	#tag : 'Model'
}

{ #category : 'visitor' }
EpPackageAddition >> accept: aVisitor [

	^ aVisitor visitPackageAddition: self
]
