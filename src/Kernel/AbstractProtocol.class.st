"
Abstract superclass for Protocol, AllProtocol should not inherit from Protocol to avoid wasting memory
"
Class {
	#name : #AbstractProtocol,
	#superclass : #Object,
	#category : #'Kernel-Protocols'
}

{ #category : #'instance creation' }
AbstractProtocol class >> ambiguous [
	^ #ambiguous
]

{ #category : #accessing }
AbstractProtocol class >> defaultName [

	^  self unclassified
]

{ #category : #testing }
AbstractProtocol class >> isAbstract [

	^ self == AbstractProtocol
]

{ #category : #accessing }
AbstractProtocol class >> nullCategory [

	^ #'no messages'
]

{ #category : #accessing }
AbstractProtocol class >> unclassified [
	^ #'as yet unclassified'
]

{ #category : #private }
AbstractProtocol >> canBeRemoved [
	^ self isEmpty
]

{ #category : #testing }
AbstractProtocol >> canBeRenamed [
	^ true
]

{ #category : #testing }
AbstractProtocol >> includesSelector: selector [

	^ self methodSelectors includes: selector
]

{ #category : #testing }
AbstractProtocol >> isEmpty [
	^ self methodSelectors isEmpty
]

{ #category : #testing }
AbstractProtocol >> isExtensionProtocol [
	^ self name first = $*
]

{ #category : #testing }
AbstractProtocol >> isVirtualProtocol [
	"A virtual protocol is a calculated one (it does not have any methods by it self)"
	^ false
]

{ #category : #accessing }
AbstractProtocol >> methodSelectors [
	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractProtocol >> name [
	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractProtocol >> name: aString [
	^ self subclassResponsibility
]

{ #category : #printing }
AbstractProtocol >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPutAll: ' (';
		nextPutAll: self name;
		nextPutAll: ') - ';
		print: self methodSelectors size;
		nextPutAll: ' selector(s)'
]
