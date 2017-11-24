Class {
	#name : #GLMTabulatorTest,
	#superclass : #TestCase,
	#category : #'Glamour-Tests-Core'
}

{ #category : #tests }
GLMTabulatorTest >> testComplex [
	| layout |
	layout := GLMTabulator new.
	layout
		addColumn: [ :column | 
			column addRow: #second.
			column addRow: #third.
			column addRow: #fourth.
			column
				addRow: [ :row | 
					row addColumn: #fifth.
					row addColumn: #sixth ] ].
	layout addColumn: #first.
	self assert: layout cell children size equals: 2.
	self assert: layout cell children first children size equals: 4.
	self assert: layout cell children first children last children size equals: 2
]

{ #category : #tests }
GLMTabulatorTest >> testOneSimpleColumnAndOneNested [
	| layout |
	layout := GLMTabulator new.
	layout
		addColumn: [ :column | 
			column addRow: #second.
			column addRow: #third.
			column addRow: #fourth ].
	layout addColumn: #first.
	self assert: layout cell children size equals: 2.
	self assert: layout cell children first children size equals: 3
]

{ #category : #tests }
GLMTabulatorTest >> testOneSimpleRowAndOneNested [
	| layout |
	layout := GLMTabulator new.
	layout addRow: #first.
	layout
		addRow: [ :row | 
			row addColumn: #second.
			row addColumn: #third.
			row addColumn: #fourth ].
	self assert: layout cell children size equals: 2.
	self assert: layout cell children last children size equals: 3
]

{ #category : #tests }
GLMTabulatorTest >> testSimpleColumns [
	| layout |
	layout := GLMTabulator new.
	layout addColumn: #first.
	layout addColumn: #second.
	self assert: layout cell children size equals: 2
]

{ #category : #tests }
GLMTabulatorTest >> testSimpleRows [
	| layout |
	layout := GLMTabulator new.
	layout addRow: #first.
	layout addRow: #second.
	self assert: layout cell children size equals: 2.
	self assert: layout cell isColumn.
	self assert: layout cell children first isRow
]
