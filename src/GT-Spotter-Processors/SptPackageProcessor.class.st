"
I am simple processor that iterates on the packages in the system
"
Class {
	#name : #SptPackageProcessor,
	#superclass : #SptSpotterProcessor,
	#category : #'GT-Spotter-Processors-Processors'
}

{ #category : #'default-settings' }
SptPackageProcessor class >> defaultEnabled [

	^ false
]

{ #category : #settings }
SptPackageProcessor class >> hideInSettings [

	"I am hidden because the implementation in the unified processor is richer"
	^ true
]

{ #category : #accessing }
SptPackageProcessor class >> order [
	
	^ 10
]

{ #category : #accessing }
SptPackageProcessor class >> title [
	^ 'Packages'
]

{ #category : #filtering }
SptPackageProcessor >> newTextFilteringSource [

	^ (SptPackageIterator new collect: [ :e | SptClassEntry on: e ]) asSubstringFilter
]
