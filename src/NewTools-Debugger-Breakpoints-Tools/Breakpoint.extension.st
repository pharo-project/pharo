Extension { #name : #Breakpoint }

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
Breakpoint >> asBreakpointInspectionItemIn: aStBreakpointInspectionItem [ 
	^aStBreakpointInspectionItem buildBreakpointItemFor: self
]

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
Breakpoint >> printContentsForBreakpointInspection [
	^node methodNode sourceCode
]

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
Breakpoint >> printTargetForBreakpointInspection [
	^node methodNode method printString
]

{ #category : #'*NewTools-Debugger-Breakpoints-Tools' }
Breakpoint >> scopeIncludes: anObject [

	self isObjectCentric ifTrue: [ ^ targetInstance == anObject ].
	^ self isObjectCentric not and: [ 
		  node methodNode methodClass = anObject class ]
]
