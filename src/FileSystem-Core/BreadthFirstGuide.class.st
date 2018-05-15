"
I traverse the filesystem in breadth-first order. Given this hierarchy:

alpha
	beta
	gamma
delta
	epsilon

I would visit the nodes in the following order:  alpha, delta, beta, gamma, epsilon.

I use my work instance variable as a queue, adding nodes to be visited to the end and retrieving them from the beginning.

"
Class {
	#name : #BreadthFirstGuide,
	#superclass : #FileSystemGuide,
	#category : #'FileSystem-Core-Base-Guide'
}

{ #category : #showing }
BreadthFirstGuide >> show: aReference [ 
	self push: aReference entry.
	self whileNotDoneDo: [ self visitNextEntry: self top ]
]

{ #category : #showing }
BreadthFirstGuide >> visitNextEntry: entry [
		
	entry isFile 
		ifTrue: [ ^ visitor visitFile: entry ].
	
	visitor visitDirectory: entry.
	
	(self shouldVisitChildrenOf: entry)
		ifTrue: [ self pushAll: entry reference entries ].
]
