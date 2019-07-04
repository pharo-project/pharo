"
I organize scheduled updates of data sources.
Idea to always defer update of browser until current UI command will be completelly done. 
It ensures that browser updates will not affect general performance of basic operations which modifies system because any update will be just queued and processes in low priority process when there will be time for this.

My single instance is created by ClyDataSource as shared class variable UpdateScheduler.
Data sources register themselfs for updates when they receives event that items were changed: 
	
	UpdateScheduler register: aDataSource.
	 
In addition actual update is executed in UI process using standart deferring logic: 

	UIManager default defer: [ next runUpdate ]

But this deferring is triggered from low priority process.

Internal Representation and Key Implementation Points.

    Instance Variables
	process:		<Process>
	updateQueue:		<AtomicSharedQueue>
"
Class {
	#name : #ClyDataSourceUpdateScheduler,
	#superclass : #Object,
	#instVars : [
		'updateQueue',
		'process'
	],
	#category : #'Calypso-Browser-DataSource'
}

{ #category : #controlling }
ClyDataSourceUpdateScheduler >> ensureRunning [
	(process notNil and: [ process isTerminated not ]) ifTrue: [ ^self].
	
	process := [ self processQueue ] 
		forkAt: Processor userBackgroundPriority + 1 named: 'Calypso update'
]

{ #category : #initialization }
ClyDataSourceUpdateScheduler >> initialize [
	super initialize.
	
	updateQueue := AtomicSharedQueue new
]

{ #category : #controlling }
ClyDataSourceUpdateScheduler >> processQueue [

	 [| available next |
        available := OrderedCollection with: updateQueue next.
        [ next := updateQueue nextOrNil ] whileNotNil: [ available add: next ].
        self runUpdateFor: available.    
    ] repeat
]

{ #category : #controlling }
ClyDataSourceUpdateScheduler >> register: aDataSource [

	updateQueue nextPut: aDataSource.
	self ensureRunning
]

{ #category : #controlling }
ClyDataSourceUpdateScheduler >> runUpdateFor: dataSources [

	UIManager default defer: [ 
		dataSources do: [ :each | each runUpdate ] ]
]
