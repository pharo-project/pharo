"
A linked list for the FreeTypeCache
"
Class {
	#name : #FreeTypeCacheLinkedList,
	#superclass : #LinkedList,
	#category : #'FreeType-Cache'
}

{ #category : #adding }
FreeTypeCacheLinkedList >> add: link after: otherLink [
	"Add otherLink  after link in the list. Answer aLink."

	| savedLink |

	savedLink := otherLink nextLink.
	otherLink nextLink: link.
	link nextLink:  savedLink.
	savedLink ifNotNil: [savedLink previousLink: link].
	link previousLink: otherLink.
	^link.
	
	
]

{ #category : #adding }
FreeTypeCacheLinkedList >> add: link before: otherLink [
	| savedLink |
	firstLink == otherLink ifTrue: [^ self addFirst: link].
	otherLink 
		ifNotNil:[
			savedLink := otherLink previousLink.
			link nextLink: otherLink.
			link previousLink: savedLink.
			otherLink previousLink: link.
			savedLink ifNotNil: [savedLink nextLink: link]].
	^ self errorNotFound: otherLink
]

{ #category : #adding }
FreeTypeCacheLinkedList >> addFirst: aLink [ 
	"Add aLink to the beginning of the receiver's list. Answer aLink."

	self isEmpty 
		ifTrue: [^lastLink :=firstLink := aLink].
	aLink nextLink: firstLink.
	aLink previousLink: nil.
	firstLink ifNotNil: [firstLink previousLink: aLink].
	firstLink := aLink.
	^aLink
]

{ #category : #adding }
FreeTypeCacheLinkedList >> addLast: aLink [ 
	"Add aLink to the end of the receiver's list. Answer aLink."

	self isEmpty 
		ifTrue: [^firstLink := lastLink := aLink].
	aLink previousLink: lastLink.
	aLink nextLink: nil.
	lastLink ifNotNil: [lastLink nextLink: aLink].
	lastLink := aLink.
	^aLink
]

{ #category : #reordering }
FreeTypeCacheLinkedList >> moveDown: aLink [
	|  e1 e2 e3 e4  |
	
	(e3 := aLink nextLink) ifNil:[^self].
	e2 := aLink.
	e4 := e3 nextLink.
	e1 := e2 previousLink.
	"swap e2 & e3"
	e1 ifNotNil:[e1 nextLink: e2].
	e2 nextLink: e3.
	e3 nextLink: e4.
	e4 ifNotNil:[e4 previousLink: e3].
	e3 previousLink: e2.
	e2 previousLink: e1

	
]

{ #category : #removing }
FreeTypeCacheLinkedList >> remove: aLink ifAbsent: aBlock [  
	| prev next  |

	prev := aLink previousLink.
	next := aLink nextLink.
	prev == nil ifFalse: [prev nextLink: next].
	next == nil ifFalse: [next previousLink: prev].
	aLink == firstLink ifTrue:[firstLink := next].
	aLink == lastLink ifTrue:[lastLink := prev].
	aLink nextLink: nil.
	aLink previousLink: nil.		
	^aLink
]

{ #category : #removing }
FreeTypeCacheLinkedList >> removeFirst [
	"Remove the first element and answer it. If the receiver is empty, create 
	an error notification."

	| oldLink |
	self emptyCheck.
	oldLink := firstLink.
	oldLink previousLink: nil.
	lastLink == firstLink 
		ifTrue:[
			lastLink := firstLink := nil.
			oldLink nextLink: nil.
			^oldLink].
	firstLink := oldLink nextLink.
	firstLink == nil 
		ifTrue:[firstLink := lastLink := nil]
		ifFalse:[firstLink previousLink: nil].
	oldLink nextLink: nil.
	^oldLink
]

{ #category : #removing }
FreeTypeCacheLinkedList >> removeLast [
	"Remove the first element and answer it. If the receiver is empty, create 
	an error notification."

	| oldLink |
	self emptyCheck.
	oldLink := lastLink.
	oldLink nextLink: nil.
	lastLink == firstLink 
		ifTrue:[
			lastLink := firstLink := nil.
			oldLink previousLink: nil.
			^oldLink].
	lastLink := oldLink previousLink.
	lastLink == nil 
		ifTrue:[firstLink := lastLink := nil]
		ifFalse:[lastLink nextLink: nil].
	oldLink previousLink: nil.
	^oldLink
]
