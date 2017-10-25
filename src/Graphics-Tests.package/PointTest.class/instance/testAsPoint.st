tests - converting
testAsPoint

	self deny: 1 isPoint.
	self deny: 1.2 isPoint.
	self assert: 1 asPoint isPoint.
	self assert: -1 asPoint isPoint.
	self assert: 1.2 asPoint isPoint.
	self assert: -1.2 asPoint isPoint.
	self assert: 1 asPoint = (1 @ 1).
	self assert: -1 asPoint = (-1 @ -1).
	self assert: 1.2 asPoint = (1.2 @ 1.2).
	self assert: -1.2 asPoint = (-1.2 @ -1.2).
	self assert: (1 @ 1) asPoint = (1 @ 1).
	self assert: (-1 @ -1) asPoint = (-1 @ -1).
	self assert: (1.2 @ 1.2) asPoint = (1.2 @ 1.2).
	self assert: (-1.2 @ -1.2) asPoint = (-1.2 @ -1.2).