"
I'm in charge to style an ast when there are metalink in the ast
"
Class {
	#name : #MetaLinkIconStyler,
	#superclass : #IconStyler,
	#category : #'Reflectivity-Tools-Breakpoints'
}

{ #category : #defaults }
MetaLinkIconStyler >> highlightColor [
	^(Color orange alpha: 0.1)
]

{ #category : #defaults }
MetaLinkIconStyler >> iconBlock: aNode [
	^ [ aNode links inspect ]
]

{ #category : #defaults }
MetaLinkIconStyler >> iconFor: aNode [
	^ self iconNamed: #smallDebugIcon 
]

{ #category : #defaults }
MetaLinkIconStyler >> iconLabel: aNode [
	^ 'Metalinks...'
]

{ #category : #testing }
MetaLinkIconStyler >> shouldStyleNode: aNode [
	 aNode hasBreakpoint ifTrue: [ ^ false ].	
	 ^ aNode hasLinks and: [ 
		   aNode links anySatisfy: [ :link | 
			   ({ 
				    Watch.
				    ExecutionCounter } includes: link metaObject class) not ] ]
]
