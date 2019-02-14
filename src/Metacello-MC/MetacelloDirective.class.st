Class {
	#name : #MetacelloDirective,
	#superclass : #Object,
	#instVars : [
		'spec',
		'loader'
	],
	#category : #'Metacello-MC-Directives'
}

{ #category : #'instance creation' }
MetacelloDirective class >> loadPackage: aPackageSpec externalReference: externalReference loader: aLoader [

	^MetacelloPackageLoadDirective new
		spec: aPackageSpec externalReference: externalReference  loader: aLoader
]

{ #category : #'instance creation' }
MetacelloDirective class >> loader: aLoader [

	^self new
		loader: aLoader
]

{ #category : #'instance creation' }
MetacelloDirective class >> postLoadSpec: packageOrVersionSpec loader: aLoader [

	^MetacelloPostLoadDirective new
		spec: packageOrVersionSpec loader: aLoader
]

{ #category : #'instance creation' }
MetacelloDirective class >> preLoadSpec: packageOrVersionSpec loader: aLoader [

	^MetacelloPreLoadDirective new
		spec: packageOrVersionSpec loader: aLoader
]

{ #category : #actions }
MetacelloDirective >> addTo: aLoaderDirective [

	aLoaderDirective add: self
]

{ #category : #enumerating }
MetacelloDirective >> directivesDo: aBlock [

	aBlock value: self
]

{ #category : #printing }
MetacelloDirective >> label [

	^self spec label
]

{ #category : #actions }
MetacelloDirective >> loadUsing: aLoaderDirective gofer: aGofer [

	self subclassResponsibility
]

{ #category : #accessing }
MetacelloDirective >> loader [

	^loader
]

{ #category : #accessing }
MetacelloDirective >> loader: aLoader [

	loader := aLoader
]

{ #category : #enumerating }
MetacelloDirective >> packageDirectivesDo: aBlock [
]

{ #category : #actions }
MetacelloDirective >> packageDo: aBlock [
]

{ #category : #actions }
MetacelloDirective >> postLoadDo: aBlock [
]

{ #category : #actions }
MetacelloDirective >> preLoadDo: aBlock [
]

{ #category : #enumerating }
MetacelloDirective >> prepostLoadDirectivesDo: aBlock [
]

{ #category : #enumerating }
MetacelloDirective >> prepostLoadDo: aBlock [
]

{ #category : #printing }
MetacelloDirective >> printOn: aStream [

	self printOn: aStream indent: 0
]

{ #category : #printing }
MetacelloDirective >> printOn: aStream indent: indent [

	indent timesRepeat: [ aStream tab ].
	aStream
		nextPutAll: self title;
		nextPutAll: ' : ';
		nextPutAll: self label.
]

{ #category : #printing }
MetacelloDirective >> printString [
	"Explicit override of superclass implementation. When you are printing a loadDirective it is annoying to have it truncated."

	^String streamContents: [:s | self printOn: s]
]

{ #category : #accessing }
MetacelloDirective >> spec [

	^spec
]

{ #category : #initialization }
MetacelloDirective >> spec: packageOrVersionSpec loader: aLoader [

	spec := packageOrVersionSpec.
	loader := aLoader
]

{ #category : #accessing }
MetacelloDirective >> title [

	self subclassResponsibility
]

{ #category : #enumerating }
MetacelloDirective >> versionDirectivesDepthFirstDo: aBlock [
]

{ #category : #enumerating }
MetacelloDirective >> versionDirectivesDo: aBlock [
]

{ #category : #actions }
MetacelloDirective >> versionDo: aBlock [
]
