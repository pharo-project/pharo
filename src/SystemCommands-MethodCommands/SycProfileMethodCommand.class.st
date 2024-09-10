"
Provides a method selection menu item to profile its execution. If the method is a regular method, it will be executed as is, without parameter provision. If the method is a test method, it will be executed within its tests class receiver through the `run:` message.

The execution should open a Profiler Presenter window with the results of the profiling.

Multiple method selections will open multiple Profiler Presenters.

If there are multiple classes selected, the command will be aborted.
"
Class {
	#name : 'SycProfileMethodCommand',
	#superclass : 'SycMethodCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'accessing' }
SycProfileMethodCommand >> defaultMenuIconName [

	^ #smallProfile
]

{ #category : 'accessing' }
SycProfileMethodCommand >> defaultMenuItemName [

	^ 'Profile method'
]

{ #category : 'execution' }
SycProfileMethodCommand >> execute [

	methods do: [ : selector | self profileMethod: selector ].

]

{ #category : 'execution' }
SycProfileMethodCommand >> newProfilerPresenter [

	^ self profilerPresenterClass new
]

{ #category : 'execution' }
SycProfileMethodCommand >> profileMethod: aCompiledMethod [ 
	
	aCompiledMethod isTestMethod
		ifTrue: [ self profileTestMethod: aCompiledMethod ]
		ifFalse: [ self profileRegularMethod: aCompiledMethod  ]
]

{ #category : 'execution' }
SycProfileMethodCommand >> profileRegularMethod: aCompiledMethod [ 

	self newProfilerPresenter
		open;
		profileBlock: [ aCompiledMethod valueWithReceiver: aCompiledMethod methodClass arguments: #(  ) ]
		displayCode: (self sourceCodeToDisplay: aCompiledMethod)
]

{ #category : 'execution' }
SycProfileMethodCommand >> profileTestMethod: aCompiledMethod [

	self newProfilerPresenter
		open;
		profileBlock: [ aCompiledMethod valueWithReceiver: aCompiledMethod methodClass arguments: #(#run: ) ]
		displayCode: (self sourceCodeToDisplay: aCompiledMethod)

]

{ #category : 'execution' }
SycProfileMethodCommand >> profilerPresenterClass [

	^ StProfilerPresenter
]

{ #category : 'initialization' }
SycProfileMethodCommand >> setUpModelFromContext: aContext [

	| browser |
	
	browser := aContext browser.
	browser classSelection isMultipleSelected
		ifTrue: [ ^ CmdCommandAborted signal ].


]

{ #category : 'execution' }
SycProfileMethodCommand >> sourceCodeToDisplay: aCompiledMethod [

	^ aCompiledMethod ast body sourceCode

]
