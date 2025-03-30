Class {
	#name : 'EFFormatterTest',
	#superclass : 'EFTest',
	#category : 'EnlumineurFormatter-Tests',
	#package : 'EnlumineurFormatter-Tests'
}

{ #category : 'tests' }
EFFormatterTest >> format: aProgram [

	^ EFFormatter format: (OCParser parseExpression: aProgram)
]

{ #category : 'tests' }
EFFormatterTest >> testFormattingABlockWithAnInlineCodeKeepItInAsingleLine [

	self assert: (self format: 'emptyBlock:=[1 halt]') equals: 'emptyBlock := [ 1 halt ]'
]

{ #category : 'tests' }
EFFormatterTest >> testFormattingABlockWithSeveralLinesInsertAnIdentationIntheCurrentLayout [

	| source |
	source := 'block := [logger logAnalysisStart.
               self initialTestRun.
               self startBudget]'.

	self assert: (self format: source) equals:
'block := [
	         logger logAnalysisStart.
	         self initialTestRun.
	         self startBudget ]'
]

{ #category : 'tests' }
EFFormatterTest >> testFormattingABlockWithSeveralLinesInsertAnIdentationIntheCurrentLayoutAndDeleteItAfter [

	| source |
	source := '[
	               line a.
	               line bb.
	               line c.
	               [
		               line d.
		               line e ].
	               [
		               line f.
		               line g.
		               line h ] ]'.

	self assert: (self format: source) equals:
'[
	line a.
	line bb.
	line c.
	[
		line d.
		line e ].
	[
		line f.
		line g.
		line h ] ]'
]

{ #category : 'tests' }
EFFormatterTest >> testFormattingAnEmptyBlockKeepItWithoutlayoutTabs [

	self
		assert: (self format: 'emptyBlock := []')
		equals: 'emptyBlock := [ ]'
]
