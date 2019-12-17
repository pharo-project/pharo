Class {
	#name : #MCAncestryTest,
	#superclass : #MCTestCase,
	#category : #'Monticello-Tests-Base'
}

{ #category : #asserting }
MCAncestryTest >> assertCommonAncestorOf: leftName and: rightName in: options in: tree [
	| left right ancestor |
	left := self versionForName: leftName in: tree.
	right := self versionForName: rightName in: tree.
	
	ancestor := left commonAncestorWith: right.
	
	self assert: (options includes: ancestor name)
]

{ #category : #asserting }
MCAncestryTest >> assertCommonAncestorOf: leftName and: rightName is: ancestorName in: tree [
	self assertCommonAncestorOf: leftName and: rightName in: (Array with: ancestorName) in: tree
]

{ #category : #asserting }
MCAncestryTest >> assertNamesOf: versionInfoCollection are: nameArray [
	| names |
	names := versionInfoCollection collect: [ :ea | ea name ].

	self assert: names asArray equals: nameArray
]

{ #category : #asserting }
MCAncestryTest >> assertPathTo: aSymbol is: anArray [
	self
		assertNamesOf: (self tree allAncestorsOnPathTo: (self treeFrom: {aSymbol}))
		are: anArray
]

{ #category : #tests }
MCAncestryTest >> testCommonAncestors [
	self assertCommonAncestorOf: #a2 and: #e2 is: #a1 in: self tree.
	self assertCommonAncestorOf: #e2 and: #b3 is: #a1 in: self tree.
	self assertCommonAncestorOf: #b2 and: #e2 is: #'00' in: self tree.
	
	self assertCommonAncestorOf: #a4 and: #b5 in: #(b2 a1) in: self twoPersonTree.
	self assertCommonAncestorOf: #b5 and: #b3 is: #b2 in: self twoPersonTree.
	self assertCommonAncestorOf: #b2 and: #a4 is: #b2 in: self twoPersonTree.
	self assertCommonAncestorOf: #b2 and: #b2 is: #b2 in: self twoPersonTree.
	self assertCommonAncestorOf: #b2 and: #a1 is: #a1 in: self twoPersonTree.
	self assertCommonAncestorOf: #a1 and: #b2 is: #a1 in: self twoPersonTree.
]

{ #category : #tests }
MCAncestryTest >> testDescendants [
	| c1 a1 b3 q1 q2 c2 |
	c1 := self tree.
	a1 := self treeFrom: #(a1 #(#('00'))).
	b3 := self treeFrom: #(b3 #(#(b2 #(#(b1 #(#(b0 #(#('00'))))))) #(a1 #(#('00'))))).
	q1 := MCWorkingAncestry new addAncestor: a1.
	q2 := MCWorkingAncestry new addAncestor: q1.
	self assert: (q2 commonAncestorWith: b3) equals: a1.
	self assert: (b3 commonAncestorWith: q2) equals: a1.
	self assert: (q2 commonAncestorWith: c1) equals: a1.
	self assert: (c1 commonAncestorWith: q2) equals: a1.
	q1 addStepChild: c1.
	self assert: (q2 commonAncestorWith: c1) equals: q1.
	self assert: (c1 commonAncestorWith: q2) equals: q1.
	c2 := MCWorkingAncestry new addAncestor: c1.
	self assert: (q2 commonAncestorWith: c2) equals: q1.
	self assert: (c2 commonAncestorWith: q2) equals: q1
]

{ #category : #tests }
MCAncestryTest >> testLinearPath [
	self assertPathTo: #b1 is: #(b3 b2)
]

{ #category : #tests }
MCAncestryTest >> testPathToMissingAncestor [
	self assertEmpty: (self tree allAncestorsOnPathTo: MCVersionInfo new)
]

{ #category : #building }
MCAncestryTest >> tree [
	^ self treeFrom:
		#(c1
			((e2
				((e1
					((a1
						(('00')))))))
			(a2
				((a1
					(('00')))))
			(b3
				((b2
					((b1
						((b0
							(('00')))))))
				(a1
					(('00')))))
			(d1)))
]

{ #category : #building }
MCAncestryTest >> twoPersonTree [
	^ self treeFrom:
		#(c1
			((a4
				((a1)
				(b3
					((b2
						((a1)))))))
			(b5
				((b2
					((a1)))))))
]

{ #category : #building }
MCAncestryTest >> versionForName: name in: tree [
	(tree name = name) ifTrue: [^ tree].
	
	tree ancestors do: [:ea | (self versionForName: name in: ea) ifNotNil: [:v | ^ v]].
	
	^ nil
]
