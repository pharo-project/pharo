Class {
	#name : 'ClyPackageDefinitionTool',
	#superclass : 'ClyTextEditorToolMorph',
	#instVars : [
		'package'
	],
	#category : 'Extension-New-System-TextEditors',
	#package : 'Extension-New-System',
	#tag : 'TextEditors'
}

{ #category : 'accessing' }
ClyPackageDefinitionTool class >> browserTabActivation [

	<classAnnotation>
	^ ClyTabActivationStrategyAnnotation for: Package asCalypsoItemContext
]

{ #category : 'accessing' }
ClyPackageDefinitionTool class >> tabOrder [

	^ 1
]

{ #category : 'operations' }
ClyPackageDefinitionTool >> applyChanges [

	^ true
]

{ #category : 'accessing' }
ClyPackageDefinitionTool >> defaultIconName [

	^ #package
]

{ #category : 'accessing' }
ClyPackageDefinitionTool >> defaultTitle [

	^ package name
]

{ #category : 'accessing' }
ClyPackageDefinitionTool >> editingText [

	^ package description
]

{ #category : 'accessing' }
ClyPackageDefinitionTool >> setUpModelFromContext [

	super setUpModelFromContext.

	package := context selectedItems first actualObject
]
