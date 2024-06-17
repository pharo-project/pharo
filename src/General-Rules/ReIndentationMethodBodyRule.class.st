"
this verify that there is at least an tabulation or 3 spaces when you write in the method body.
"
Class {
	#name : 'ReIndentationMethodBodyRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Formatting',
	#package : 'General-Rules',
	#tag : 'Formatting'
}

{ #category : 'accessing' }
ReIndentationMethodBodyRule class >> group [ 

	^ 'Formatting'
]

{ #category : 'accessing' }
ReIndentationMethodBodyRule class >> ruleName [ 

	^ 'make at least an indentation when you write in the method body'
]

{ #category : 'helpers' }
ReIndentationMethodBodyRule >> anchorFor: aNode [

	^ ReIntervalSourceAnchor
			entity: aNode methodNode body
			interval: (Interval from: aNode methodNode body start to: aNode stop)
]
