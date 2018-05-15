"
I traverse the filesystem in depth-first pre order. Given this hierarchy:

alpha
	beta
	gamma
delta
	epsilon

I would visit the nodes in the following order: alpha, beta, gamma, delta, epsilon.

I use my work instance variable as a stack. I push nodes to be visited and visit them in reverse order.
"
Class {
	#name : #PreorderGuide,
	#superclass : #FileSystemGuide,
	#category : #'FileSystem-Core-Base-Guide'
}

{ #category : #showing }
PreorderGuide >> pushAll: aCollection [ 
	aCollection reverseDo: [ :each | work add: each ]
]

{ #category : #showing }
PreorderGuide >> show: aReference [
	self push: aReference entry.
	self whileNotDoneDo: [ self visitNextEntry: self pop ]
]

{ #category : #showing }
PreorderGuide >> visitNextEntry: entry [
	
	entry isFile
		ifTrue: [ visitor visitFile: entry ]
		ifFalse: [ 
			visitor visitDirectory: entry.
			(self shouldVisitChildrenOf: entry)
				ifTrue: [ self pushAll: entry reference entries ]]
]
