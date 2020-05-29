"
I am a test class to measure the impact of the new implementation of spotter processors.
"
Class {
	#name : #BenchProcessors,
	#superclass : #Object,
	#category : #'GT-Tests-Spotter-Processors'
}

{ #category : #benchmarking }
BenchProcessors >> benchClassFinding [
	<script: 'self new benchClassFinding'>

	| numberOfClasses |
	
	numberOfClasses := SystemNavigation default allBehaviors size.

	(String streamContents: [ :aStream |
		aStream 
			<< 'Classes'; cr;
			<< '-------'; cr;
			cr;
			<< 'Total Classes:';
			print: numberOfClasses;
			cr;cr.

		self runClassBenchMarksFor: 'znu' resultsInto: aStream for: #ClassProcessor.	
		self runClassBenchMarksFor: 'ast' resultsInto: aStream for: #ClassProcessor.	
		self runClassBenchMarksFor: 'gtspotter' resultsInto: aStream for: #ClassProcessor.	
		self runClassBenchMarksFor: 'test' resultsInto: aStream for: #ClassProcessor.	
	]) inspect.

]

{ #category : #benchmarking }
BenchProcessors >> benchImplementors [
	<script: 'self new benchImplementors'>

	| numberOfMethods |
	
	numberOfMethods := SystemNavigation default allBehaviors sum: [:c | c localMethods size].

	(String streamContents: [ :aStream |
		aStream 
			<< 'Methods'; cr;
			<< '-------'; cr;
			cr;
			<< 'Total Methods:';
			print: numberOfMethods;
			cr;cr.

		self runClassBenchMarksFor: 'asString' resultsInto: aStream for: #ImplementorsProcessor.	
		self runClassBenchMarksFor: 'printOn:' resultsInto: aStream for: #ImplementorsProcessor.	
		self runClassBenchMarksFor: 'asUrl' resultsInto: aStream for: #ImplementorsProcessor.	
		self runClassBenchMarksFor: 'test' resultsInto: aStream for: #ImplementorsProcessor.	
	]) inspect.

]

{ #category : #benchmarking }
BenchProcessors >> contextForText: aString [

	| candidates stream |

	candidates := GTMockCandidatesList new.
	stream := GTMockSpotterStream new
		receiver: candidates;
		onAddedSelector: #addObject:inProcessor:;
		yourself.

	^ GTSpotterContext new 
		step: self;
		stream: stream;
		text: aString;
		search: aString;
		yourself.

	
]

{ #category : #benchmarking }
BenchProcessors >> newClassProcessor [

	^ SptClassProcessor new
]

{ #category : #benching }
BenchProcessors >> newImplementorsProcessor [
	
	^ SptImplementorsProcessor new
]

{ #category : #benchmarking }
BenchProcessors >> oldClassProcessor [

	^ GTSpotterCandidatesListProcessor new
			allCandidates: [ Smalltalk allClassesAndTraits ];
			title: 'Classes';
			filter: GTFilterFuzzy;
			itemIcon: #systemIcon;
			keyBinding: $b meta;
			wantsToDisplayOnEmptyQuery: false;
			yourself.

]

{ #category : #benching }
BenchProcessors >> oldImplementorsProcessor [

	^ GTSpotterCandidatesListProcessor new
		title: 'Implementors';
		filter: GTFilterImplementor item: [ :filter :context | 
			SystemNavigation default allBehaviorsDo: [ :class | class methodsDo: filter ] ];
		keyBinding: $m meta;
		wantsToDisplayOnEmptyQuery: false
]

{ #category : #benching }
BenchProcessors >> runClassBenchMarksFor: aString resultsInto: aWriteStream for: processorName [
	| context oldProcessor newProcessor oldBench newBench |
	
	oldBench := [
		context := self contextForText: aString.
		oldProcessor := self perform: ('old' , processorName) asSymbol.
		oldProcessor filterInContext: context] bench.

	newBench := [
		context := self contextForText: aString.
		newProcessor := self perform: ('new' , processorName) asSymbol.
		newProcessor filterInContext: context] bench.

	aWriteStream
			<< 'Searching:';
			print: aString; tab; tab; tab;
			<< 'Old Implementation:'; 
			<< oldBench; tab;	
			<< 'new Implementation:'; 
			<< newBench; cr.

]
