"
I represent a package creation in the system.
"
Class {
	#name : #EpPackageAddition,
	#superclass : #EpCategoryChange,
	#category : #'Epicea-Model'
}

{ #category : #visitor }
EpPackageAddition >> accept: aVisitor [

	^ aVisitor visitPackageAddition: self
]

{ #category : #accessing }
EpPackageAddition >> categoryName [

	self flag: #epicea. "Should be removed when the inst var will not be here anymore."
	self shouldNotImplement
]
