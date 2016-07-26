tests
testDescendants
	| c1 a1 b3 q1 q2 c2 |
	c1 := self tree.
	a1 := self treeFrom: #(a1 (('00'))).
	b3 := self treeFrom: #(b3
				((b2
					((b1
						((b0
							(('00')))))))
				(a1
					(('00'))))).
	q1 := MCWorkingAncestry new addAncestor: a1.
	q2 := MCWorkingAncestry new addAncestor: q1.
	self assert: (q2 commonAncestorWith: b3) = a1.
	self assert: (b3 commonAncestorWith: q2) = a1.
	self assert: (q2 commonAncestorWith: c1) = a1.
	self assert: (c1 commonAncestorWith: q2) = a1.
	q1 addStepChild: c1.
	self assert: (q2 commonAncestorWith: c1) = q1.
	self assert: (c1 commonAncestorWith: q2) = q1.
	c2 := MCWorkingAncestry new addAncestor: c1.
	self assert: (q2 commonAncestorWith: c2) = q1.
	self assert: (c2 commonAncestorWith: q2) = q1.
