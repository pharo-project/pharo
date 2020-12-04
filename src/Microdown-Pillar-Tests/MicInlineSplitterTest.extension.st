Extension { #name : #MicInlineSplitterTest }

{ #category : #'*Microdown-Pillar-Tests' }
MicInlineSplitterTest >> testBasicTextAsPillar [
	| res |
	res := self splitter pillarFrom: 'abc'.
	self assert: res first text equals: 'abc'.
	self assert: res first class equals: PRText.
]

{ #category : #'*Microdown-Pillar-Tests' }
MicInlineSplitterTest >> testBoldAndNestedItalicAsPillar [
	| res |
	res := self splitter pillarFrom: 'abc**x_y_z**cba'.
	self assert: res second class equals: PRBoldFormat.
	self assert: res second children first text equals: 'x'.
	self assert: res second children second class equals: PRItalicFormat.
	self assert: res second children second children first text equals: 'y'.
	self assert: res second children third text equals: 'z'
]

{ #category : #'*Microdown-Pillar-Tests' }
MicInlineSplitterTest >> testBoldAsPillar [
	| res |
	res := self splitter pillarFrom: 'abc**xyz**cba'.
	self assert: res second children first text equals: 'xyz'.
	self assert: res second class equals: PRBoldFormat.
]

{ #category : #'*Microdown-Pillar-Tests' }
MicInlineSplitterTest >> testExternalLinkAsPillar [
	| res |
	res := self splitter pillarFrom: 'abc[alias](link)cba'.
	self assert: res second children first text equals: 'alias'.
	self assert: res second class equals: PRExternalLink.

]

{ #category : #'*Microdown-Pillar-Tests' }
MicInlineSplitterTest >> testFigureAsPillar [
	| res |
	res := self splitter pillarFrom: 'abc![alias](link)cba'.
	self assert: res second children first text equals: 'alias'.
	self assert: res second class equals: PRFigure

]

{ #category : #'*Microdown-Pillar-Tests' }
MicInlineSplitterTest >> testItalicAsPillar [
	| res |
	res := self splitter pillarFrom: 'abc_xyz_cba'.
	self assert: res second children first text equals: 'xyz'.
	self assert: res second class equals: PRItalicFormat.
	
	
]
