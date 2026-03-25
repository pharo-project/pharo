Class {
	#name : 'AnalyserA',
	#superclass : 'Object',
	#category : 'HeuristicCompletion-Analyser-Data',
	#package : 'HeuristicCompletion-Analyser-Data'
}

{ #category : 'operations' }
AnalyserA >> defineInDependentOfClassWithDependencyOnData2Package [
   "I'm a method used for the test to check that local methods are invoked before this one."

   ^ 42



]

{ #category : 'operations' }
AnalyserA >> zznotLocallyDefinedInClassWithDependencyOnData2Package [
	"I'm a method used for the test to check that dependent methods are invoked when there is no local one."

	^ 42
]
