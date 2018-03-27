"
This class changes the default compiler used to compile itself to include the AST transforming plugin ASTPluginMeaningOfLife.

#example42 does not mean what you think it means (see bytecode)
"
Class {
	#name : #ASTTransformExamplePluginActive,
	#superclass : #Object,
	#category : #'OpalCompiler-Tests-Plugins'
}

{ #category : #compiler }
ASTTransformExamplePluginActive class >> compiler [
	"the example plugin is active for this class"
	^super compiler addPlugin: ASTPluginMeaningOfLife.
]

{ #category : #example }
ASTTransformExamplePluginActive >> example42 [
	<sampleInstance>
	^42
]
