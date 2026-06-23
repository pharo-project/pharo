Class {
	#name : 'ClapDbgPrintCodeCommand',
	#superclass : 'ClapDbgCommand',
	#category : 'Debugger-CLI',
	#package : 'Debugger-CLI'
}

{ #category : 'execution' }
ClapDbgPrintCodeCommand >> executeOn: aDebugger [

	^ self executeOn: aDebugger with: ''
]

{ #category : 'execution' }
ClapDbgPrintCodeCommand >> executeOn: aDebugger with: args [

	| codeOfCurrentMethod pcRange debugContext |
	debugContext := aDebugger context.
	codeOfCurrentMethod := debugContext method sourceCode asString.

	pcRange := aDebugger session pcRangeForContext: debugContext.
    
    ^ '{1}--->{2}<---{3}' format: { 
        (codeOfCurrentMethod copyFrom: 1 to: pcRange first - 1).
        (codeOfCurrentMethod copyFrom: pcRange first to: pcRange last).
        (codeOfCurrentMethod copyFrom: pcRange last + 1 to: codeOfCurrentMethod size) }
]

{ #category : 'initialization' }
ClapDbgPrintCodeCommand >> initialize [

	super initialize.
	self commandName: #( 'printCode' 'pc' ).
	self commandHelp: 'print current''method code'.
	self arguments: ''
]
