"
Runs all covering tests of selected methods.
"
Class {
	#name : 'ClyRunCoveringTestMethodsCommand',
	#superclass : 'ClyRunTestsFromMethodsCommand',
	#category : 'Calypso-SystemPlugins-SUnit-Browser-Commands',
	#package : 'Calypso-SystemPlugins-SUnit-Browser',
	#tag : 'Commands'
}

{ #category : 'testing' }
ClyRunCoveringTestMethodsCommand >> canBeExecuted [

	^ self context selectedMethodItems anySatisfy: [ :each | each hasProperty: ClyTestedMethodProperty ]
]

{ #category : 'accessing' }
ClyRunCoveringTestMethodsCommand >> testItems [
	^ testItems ifNil: [ self context ifNotNil: [ :ctx | ctx selectedItems select: [ :each | each hasProperty: ClyTestedMethodProperty ] ] ]
]

{ #category : 'execution' }
ClyRunCoveringTestMethodsCommand >> testResultOf: methodItem [
	^(methodItem getProperty: ClyTestedMethodProperty) testResult
]

{ #category : 'execution' }
ClyRunCoveringTestMethodsCommand >> testSelectorOf: methodItem [

	^(methodItem getProperty: ClyTestedMethodProperty) coveringTest selector
]
