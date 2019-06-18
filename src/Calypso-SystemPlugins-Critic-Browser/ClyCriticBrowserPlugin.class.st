"
I am browser plugin.
I decorate browser tools with critiques information about their subjects.
This information is shown in bottom pane of the tool as a table of related critiques
"
Class {
	#name : #ClyCriticBrowserPlugin,
	#superclass : #ClyBrowserPlugin,
	#category : #'Calypso-SystemPlugins-Critic-Browser'
}

{ #category : #'tools installing' }
ClyCriticBrowserPlugin >> decorateTool: aBrowserTool [

	aBrowserTool supportsCriticDecoration ifFalse: [ ^self ].
		
	aBrowserTool addDecorator: ClyCriticDecorator new.
]

{ #category : #testing }
ClyCriticBrowserPlugin >> isSlow [
	^true
]

{ #category : #accessing }
ClyCriticBrowserPlugin >> priority [
	^SmallInteger maxVal 
]
