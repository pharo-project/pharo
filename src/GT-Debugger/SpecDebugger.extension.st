Extension { #name : #SpecDebugger }

{ #category : #'*GT-Debugger' }
SpecDebugger class >> availableAutomatically [

	^ false
]

{ #category : #'*GT-Debugger' }
SpecDebugger class >> handlesContext: aContext [

	^ true
]

{ #category : #'*GT-Debugger' }
SpecDebugger class >> spanNewSessionFrom: anotherSession [

	^ self sessionClass 
		named: anotherSession name 
		on: anotherSession interruptedProcess 
		startedAt: anotherSession interruptedContext
]
