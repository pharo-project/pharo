"
This rule evaluate an approximation of the cycolmatic complexity of a method. She count the block number in methods  
"
Class {
	#name : 'ReCyclomaticComplexityRule',
	#superclass : 'ReAbstractRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'testing - interest' }
ReCyclomaticComplexityRule class >> checksMethod [ 

	^ true
]

{ #category : 'accessing' }
ReCyclomaticComplexityRule class >> group [ 

	^ 'Optimization'
]

{ #category : 'accessing' }
ReCyclomaticComplexityRule class >> rationale [ 

	^ 'When the cyclomatic complexity is heigh, try to refactor your code'
]

{ #category : 'accessing' }
ReCyclomaticComplexityRule class >> ruleName [

	^ 'The cyclomatic complexity is heigh'
]

{ #category : 'running' }
ReCyclomaticComplexityRule >> basicCheck: aMethod [
	
	^ aMethod blockNodes size > self complexityMax 
]

{ #category : 'private' }
ReCyclomaticComplexityRule >> complexityMax [

	^ 10
]
