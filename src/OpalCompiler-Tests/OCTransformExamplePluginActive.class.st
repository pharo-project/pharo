"
This class changes the default compiler used to compile itself to include the AST transforming plugin ASTPluginMeaningOfLife.

#example42 does not mean what you think it means (see bytecode)
"
Class {
	#name : 'OCTransformExamplePluginActive',
	#superclass : 'Object',
	#category : 'OpalCompiler-Tests-Plugins',
	#package : 'OpalCompiler-Tests',
	#tag : 'Plugins'
}

{ #category : 'compiler' }
OCTransformExamplePluginActive class >> compiler [
	"the example plugin is active for this class"
	^super compiler addPlugin: OCPluginMeaningOfLife
]

{ #category : 'examples' }
OCTransformExamplePluginActive >> example42 [
	<sampleInstance>
	^42
]
