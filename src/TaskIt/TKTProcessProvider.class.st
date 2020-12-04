"
I am a process provider. I define how processes are created and provide a hook for those how would like to have more control on how it is done.
"
Class {
	#name : #TKTProcessProvider,
	#superclass : #Object,
	#category : #'TaskIt-Kernel'
}

{ #category : #'instance creation' }
TKTProcessProvider class >> createProcessDoing: aValuable [
	^ self error: 'Use TKTConfiguration processProvider createProcessDoing: aValuable '
]

{ #category : #'instance creation' }
TKTProcessProvider class >> createProcessDoing: aValuable named: aName [
	^ self error: 'Use TKTConfiguration processProvider createProcessDoing: aValuable named: aName'
]

{ #category : #accessing }
TKTProcessProvider class >> current [

	^ self error: 'Use TKTConfiguration processProvider createProcessDoing: aValuable '
]

{ #category : #accessing }
TKTProcessProvider class >> current: aProcessProvider [
	^ self error: 'Use a configuration'
]

{ #category : #'instance creation' }
TKTProcessProvider >> createProcessDoing: aValuable [

	self subclassResponsibility
]

{ #category : #'instance creation' }
TKTProcessProvider >> createProcessDoing: aValuable named: aName [

	self subclassResponsibility
]

{ #category : #'instance creation' }
TKTProcessProvider >> isDebuggingCompatible [
	^ false
]
