"
PLEASE USE LibrarySymbolNotFoundError from Alien package now
 
This class here is for compatibility as packages like DataFrame or other
need to switch from NotFoundError to LibrarySymbolNotFoundError
(see http://lists.squeakfoundation.org/pipermail/vm-dev/2019-June/031098.html)
"
Class {
	#name : #NotFoundError,
	#superclass : #LibrarySymbolNotFoundError,
	#category : #Deprecated80
}

{ #category : #testing }
NotFoundError class >> isDeprecated [
	^true
]
