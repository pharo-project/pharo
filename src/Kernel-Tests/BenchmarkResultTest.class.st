Class {
	#name : #BenchmarkResultTest,
	#superclass : #TestCase,
	#category : 'Kernel-Tests-Chronology'
}

{ #category : #tests }
BenchmarkResultTest >> testEmpty [
	| benchmarkResult |
	benchmarkResult := BenchmarkResult new.
	self assert: benchmarkResult isEmpty.
	self assert: benchmarkResult iterations isNil.
	self assert: benchmarkResult elapsedTime isNil.
	"cover the code at least"
	self deny: benchmarkResult shortPrintString isEmpty.
	self deny: benchmarkResult printString isEmpty

]

{ #category : #tests }
BenchmarkResultTest >> testOne [
	| benchmarkResult |
	benchmarkResult := BenchmarkResult new.
	benchmarkResult iterations: 1.
	benchmarkResult elapsedTime: 1 second.
	self deny: benchmarkResult isEmpty.
	self assert: benchmarkResult iterations equals: 1.
	self assert: benchmarkResult elapsedTime equals: 1 second.
	self assert: benchmarkResult frequency equals: 1.
	self assert: benchmarkResult executionsPerSecond equals: 1.
	self assert: benchmarkResult period equals: 1 second.
	self assert: benchmarkResult timePerExecution equals: 1 second.
	"cover the code at least"
	self deny: benchmarkResult shortPrintString isEmpty.
	self deny: benchmarkResult printString isEmpty
	

]

{ #category : #tests }
BenchmarkResultTest >> testOneOverTwo [
	| benchmarkResult |
	benchmarkResult := BenchmarkResult new.
	benchmarkResult iterations: 1.
	benchmarkResult elapsedTime: 2 second.
	self deny: benchmarkResult isEmpty.
	self assert: benchmarkResult iterations equals: 1.
	self assert: benchmarkResult elapsedTime equals: 2 second.
	self assert: benchmarkResult frequency equals: 0.5.
	self assert: benchmarkResult executionsPerSecond equals: 0.5.
	self assert: benchmarkResult period equals: 2 second.
	self assert: benchmarkResult timePerExecution equals: 2 second.
	"cover the code at least"
	self assert: benchmarkResult shortPrintString equals: '0.500 per second'.
	self deny: benchmarkResult printString isEmpty
	

]

{ #category : #tests }
BenchmarkResultTest >> testSimple [
	| benchmarkResult |
	benchmarkResult := BenchmarkResult new.
	benchmarkResult iterations: 100.
	benchmarkResult elapsedTime: 4 seconds.
	self deny: benchmarkResult isEmpty.
	self assert: benchmarkResult iterations equals: 100.
	self assert: benchmarkResult elapsedTime equals: 4 seconds.
	self assert: benchmarkResult frequency equals: 25.
	self assert: benchmarkResult executionsPerSecond equals: 25.
	self assert: benchmarkResult period equals: 40 milliSeconds.
	self assert: benchmarkResult timePerExecution equals: 40 milliSeconds.
	"cover the code at least"
	self assert: benchmarkResult shortPrintString equals: '25.000 per second'.
	self deny: benchmarkResult printString isEmpty
	

]
