"
Provides a test method selection menu item to profile its execution. The selected test method will be executed within its tests class receiver through the `run:` message.

The execution should open a Profiler Presenter window with the results of the profiling.

Multiple method selections will open multiple Profiler Presenters.

If there are multiple classes selected, the command will be aborted.
"
Class {
	#name : 'SycProfileTestMethodCommand',
	#superclass : 'SycMethodCommand',
	#category : 'SystemCommands-MethodCommands',
	#package : 'SystemCommands-MethodCommands'
}

{ #category : 'accessing' }
SycProfileTestMethodCommand >> defaultMenuIconName [

	^ #smallProfile
]

{ #category : 'accessing' }
SycProfileTestMethodCommand >> defaultMenuItemName [

	^ 'Profile method'
]

{ #category : 'execution' }
SycProfileTestMethodCommand >> execute [

	methods 
		select: [ : cm | cm isTestMethod ]
		thenDo: [ : cm | self profileMethod: cm ].

]

{ #category : 'execution' }
SycProfileTestMethodCommand >> newProfilerPresenter [

	^ self profilerPresenterClass new
]

{ #category : 'execution' }
SycProfileTestMethodCommand >> profileMethod: aCompiledMethod [ 
	
	self newProfilerPresenter
		open;
		profileBlock: [ 
			(TestCase class >> #run:) 
				valueWithReceiver: aCompiledMethod methodClass 
				arguments: { aCompiledMethod selector } ]
		displayCode: (self sourceCodeToDisplay: aCompiledMethod)
]

{ #category : 'execution' }
SycProfileTestMethodCommand >> profilerPresenterClass [

	^ Smalltalk
		at: #StProfilerPresenter
		ifAbsent: [ self error: 'Profiler not installed' ]
]

{ #category : 'initialization' }
SycProfileTestMethodCommand >> setUpModelFromContext: aContext [

	| browser |
	
	browser := aContext browser.
	browser classSelection isMultipleSelected
		ifTrue: [ ^ CmdCommandAborted signal ].


]

{ #category : 'execution' }
SycProfileTestMethodCommand >> sourceCodeToDisplay: aCompiledMethod [

	^ aCompiledMethod ast body sourceCode

]
