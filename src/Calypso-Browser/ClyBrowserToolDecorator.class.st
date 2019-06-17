"
I a base class for various kinds of browser tab decorators.
Decorators are added to browser tools by browser plugins using following method: 

	aBrowserTool addDecorator: aBrowserToolDecorator
	
Subclasses should implement single method: 

- decorateTool

When decorator is added to the tool it sets my browserTool variable.
And #decorateTool method performs required logic with this given tool.
 
Internal Representation and Key Implementation Points.

    Instance Variables
	browserTool:		<ClyBrowserTool>
"
Class {
	#name : #ClyBrowserToolDecorator,
	#superclass : #Object,
	#instVars : [
		'browserTool'
	],
	#category : #'Calypso-Browser-UI'
}

{ #category : #accessing }
ClyBrowserToolDecorator >> browser [
	^ browserTool browser
]

{ #category : #accessing }
ClyBrowserToolDecorator >> browserEnvironment [
	^ self browser navigationEnvironment 
]

{ #category : #accessing }
ClyBrowserToolDecorator >> browserTool [
	^ browserTool
]

{ #category : #accessing }
ClyBrowserToolDecorator >> browserTool: anObject [
	browserTool := anObject
]

{ #category : #decoration }
ClyBrowserToolDecorator >> decorateTool [
	self subclassResponsibility 
]

{ #category : #controlling }
ClyBrowserToolDecorator >> toolWasRemoved [

]
