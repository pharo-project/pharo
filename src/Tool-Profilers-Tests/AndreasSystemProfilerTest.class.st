"
SUnit tests for AndreasSystemProfiler
"
Class {
	#name : #AndreasSystemProfilerTest,
	#superclass : #TestCase,
	#instVars : [
		'tally'
	],
	#category : #'Tool-Profilers-Tests-System'
}

{ #category : #running }
AndreasSystemProfilerTest >> setUp [
	super setUp.
	tally := QSystemTally new.
	tally class: self class method: self class >> #testPrintingTally	
	"tally class: Object method: Object >> #printString."
]

{ #category : #'tests tally' }
AndreasSystemProfilerTest >> testFullPrintOn [
	| stream |
	stream := WriteStream on: String new.
	tally fullPrintOn: stream tallyExact: false orThreshold: 0 time: 10.
	
	self assert: stream contents equals: '**Tree**

**Leaves**
'
]

{ #category : #'tests tally' }
AndreasSystemProfilerTest >> testGetNewTab [
	| tabs |
	
	self assert: (tally getNewTabsFor: #()) equals: #().
	self assert: (tally getNewTabsFor: #(' ')) equals:  #(' ').
	
	tabs := OrderedCollection new.
	tabs add: '['.
	(tally maxTabs + 1) timesRepeat: [ tabs add: ' ' ].
	self assert: (tally getNewTabsFor: tabs) asArray equals: #('[' '[')
]

{ #category : #'tests tally' }
AndreasSystemProfilerTest >> testPrintingTally [

	self assert: tally printString equals: 'AndreasSystemProfilerTest>>#testPrintingTally -- 0'
]

{ #category : #'tests tally' }
AndreasSystemProfilerTest >> testPrintingTallyUninitialized [
	
	self assert: QSystemTally new printString equals: 'nil>>nil -- nil'
]

{ #category : #'tests profiler' }
AndreasSystemProfilerTest >> testSimple [

	AndreasSystemProfiler new spyOn: [ 200 timesRepeat: [ 1.23 printString ]]
]

{ #category : #'tests profiler' }
AndreasSystemProfilerTest >> testSimpleReport [

	AndreasSystemProfiler new report
]

{ #category : #'tests tally' }
AndreasSystemProfilerTest >> testTallyTreePrint [
	| stream |
	stream := WriteStream on: String new.
	tally 
		treePrintOn: stream 
		tabs: #() 
		thisTab: nil 
		total: 50 
		totalTime: 100 
		tallyExact: true 
		orThreshold: 1.
	
	"Nothing is printed since there is no tab"
	self assertEmpty: stream contents
]

{ #category : #'tests tally' }
AndreasSystemProfilerTest >> testTallyTreePrintWithTab [
	| stream |
	stream := WriteStream on: String new.
	tally 
		treePrintOn: stream 
		tabs: #(' ')
		thisTab: nil 
		total: 50 
		totalTime: 100 
		tallyExact: true 
		orThreshold: 1.	
	
	self assert: stream contents equals: ' 0 AndreasSystemProfilerTest  testPrintingTally
'
]
