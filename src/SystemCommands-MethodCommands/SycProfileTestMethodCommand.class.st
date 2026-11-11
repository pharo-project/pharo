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

{ #category : 'execution' }
SycProfileTestMethodCommand >> executeRefactoring [

	methods
		select: [ :cm | cm isTestMethod ]
		thenDo: [ :cm | self profileMethod: cm ]
]

{ #category : 'accessing' }
SycProfileTestMethodCommand >> iconName [

	^ #smallProfile
]

{ #category : 'accessing' }
SycProfileTestMethodCommand >> name [

	^ 'Profile method'
]

{ #category : 'execution' }
SycProfileTestMethodCommand >> newProfilerPresenter [

	^ self profilerPresenterClass new
]

{ #category : 'accessing' }
SycProfileTestMethodCommand >> order [

	^ 1000
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

{ #category : 'execution' }
SycProfileTestMethodCommand >> sourceCodeToDisplay: aCompiledMethod [

	^ aCompiledMethod ast body sourceCode

]
