"
SUnit tests for AndreasSystemProfiler
"
Class {
	#name : #AndreasSystemProfilerTest,
	#superclass : #TestCase,
	#instVars : [
		'tally'
	],
	#category : #Tools-Test-Profilers
}

{ #category : #running }
AndreasSystemProfilerTest >> setUp [
	tally := QSystemTally new.
	tally class: self class method: self class >> #testPrintingTally	
	"tally class: Object method: Object >> #printString."
]

{ #category : #'tests tally' }
AndreasSystemProfilerTest >> testFullPrintOn [
	| stream |
	stream := WriteStream on: String new.
	tally 
		fullPrintOn: stream tallyExact: false orThreshold: 0 time: 10.
	
	self assert: stream contents =  '**Tree**

**Leaves**
'
]

{ #category : #'tests tally' }
AndreasSystemProfilerTest >> testGetNewTab [
	| tabs |
	
	self assert: (tally getNewTabsFor: #()) = #().
	self assert: (tally getNewTabsFor: #(' ')) =  #(' ').
	
	tabs := OrderedCollection new.
	tabs add: '['.
	(tally maxTabs + 1) timesRepeat: [ tabs add: ' ' ].
	self assert: (tally getNewTabsFor: tabs) asArray =  #('[' '[').
]

{ #category : #'tests tally' }
AndreasSystemProfilerTest >> testPrintingTally [

	self assert: tally printString =  'AndreasSystemProfilerTest>>#testPrintingTally -- 0'
]

{ #category : #'tests tally' }
AndreasSystemProfilerTest >> testPrintingTallyUninitialized [
	
	self assert: QSystemTally new printString = 'nil>>nil -- nil'
]

{ #category : #'tests profiler' }
AndreasSystemProfilerTest >> testSimple [
	AndreasSystemProfiler new spyOn: [ 200 timesRepeat: [ 1.23 printString ] ]
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
	self assert: stream contents isEmpty
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
	
	
	self assert: stream contents = ' 0 AndreasSystemProfilerTest  testPrintingTally
'
]
