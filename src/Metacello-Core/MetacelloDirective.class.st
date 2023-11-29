Class {
	#name : 'MetacelloDirective',
	#superclass : 'Object',
	#instVars : [
		'spec'
	],
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'instance creation' }
MetacelloDirective class >> loadPackage: aPackageSpec externalReference: externalReference [

	^ MetacelloPackageLoadDirective new spec: aPackageSpec externalReference: externalReference
]

{ #category : 'instance creation' }
MetacelloDirective class >> postLoadSpec: packageOrVersionSpec [

	^ MetacelloPostLoadDirective new spec: packageOrVersionSpec
]

{ #category : 'instance creation' }
MetacelloDirective class >> preLoadSpec: packageOrVersionSpec [

	^ MetacelloPreLoadDirective new spec: packageOrVersionSpec
]

{ #category : 'visiting' }
MetacelloDirective >> acceptVisitor: aVisitor [
	
	aVisitor visitAtomicLoadDirective: self
]

{ #category : 'actions' }
MetacelloDirective >> addTo: aLoaderDirective [

	aLoaderDirective add: self
]

{ #category : 'actions' }
MetacelloDirective >> executeUsing: aTarget [

	self subclassResponsibility
]

{ #category : 'printing' }
MetacelloDirective >> label [

	^self spec label
]

{ #category : 'enumerating' }
MetacelloDirective >> packageDirectivesDo: aBlock [
]

{ #category : 'printing' }
MetacelloDirective >> printOn: aStream [

	self printOn: aStream indent: 0
]

{ #category : 'printing' }
MetacelloDirective >> printOn: aStream indent: indent [

	indent timesRepeat: [ aStream tab ].
	aStream
		nextPutAll: self title;
		nextPutAll: ' : ';
		nextPutAll: self label.
]

{ #category : 'printing' }
MetacelloDirective >> printString [
	"Explicit override of superclass implementation. When you are printing a loadDirective it is annoying to have it truncated."

	^String streamContents: [:s | self printOn: s]
]

{ #category : 'accessing' }
MetacelloDirective >> spec [

	^spec
]

{ #category : 'initialization' }
MetacelloDirective >> spec: packageOrVersionSpec [

	spec := packageOrVersionSpec.
]

{ #category : 'accessing' }
MetacelloDirective >> title [

	self subclassResponsibility
]
